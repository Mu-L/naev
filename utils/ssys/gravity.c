
#include <libgen.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define THREADS 4
#define GRAV_FACT 0.1f

// defaults to 1.0
const struct {
   char *nam;
   float w;
} weights[] = {
   {"anubis_black_hole", 5.5f}, {NULL} // Sentinel
};

const float fact = 30.0f;
const float _rad = 30.0f;
// See HERE for their computation
float rad, mul_ct, add_ct, inv_fact;

static inline float _pot(float xs, float ys, float x, float y)
{
   const float dx = xs - x;
   const float dy = ys - y;
   const float d  = sqrt(dx * dx + dy * dy) * inv_fact;

   if (d < rad)
      return d * d * mul_ct + add_ct;
   else
      return -1.0f / d;
}

// The derivative of the previous one.
static inline void accum_f(float *vx, float *vy, float xs, float ys, float x,
                           float y, float m)
{
   float dx = xs - x;
   float dy = ys - y;
   float d  = sqrt(dx * dx + dy * dy);
   float f;

   dx /= d;
   dy /= d;

   d *= inv_fact;
   if (d < rad)
      f = 2.0f * d * mul_ct;
   else
      f = 1.0f / d * d;

   f *= m;
   *vx += f * dx;
   *vy += f * dy;
}

void output_map_header(FILE *fp, int w, int h)
{
   char b[64];

   fwrite(b, sizeof(char), snprintf(b, 63, "P5 %d %d 65535\n", w, h), fp);
}

void output_map(FILE *fp, float *map, int w, int h, float fact, float ct)
{
   uint8_t *line;

   output_map_header(fp, w, h);

   line = (uint8_t *) calloc((size_t) w, 2 * sizeof(uint8_t));
   for (int i = 0; i < h; i++) {
      for (int j = 0; j < w; j++) {
         const unsigned u = 65535.0f * (fact * map[(h - i - 1) * w + j] + ct);
         line[2 * j]      = u >> 8;
         line[2 * j + 1]  = u & 0xff;
      }
      fwrite(line, sizeof(uint16_t), w, fp);
   }
   fclose(fp);
   free(line);
}

void gen_potential(float *lst, size_t nb, float scale)
{
   static float *maps[THREADS - 1];
   static int   *percent;
   char          buf[32] = {[0] = '\r'};
   float        *map;
   float         minx, maxx, miny, maxy;
   size_t        n;
   float         min_pot = 0.0; // working only with neg potentials

   if (!nb)
      return;

   for (n = 0; n < nb; n++) {
      lst[4 * n + 0] *= scale;
      lst[4 * n + 1] *= scale;
   }

   minx = maxx = lst[0];
   miny = maxy = lst[1];
   for (n = 1; n < nb; n++) {
      minx = lst[4 * n + 0] < minx ? lst[4 * n + 0] : minx;
      maxx = lst[4 * n + 0] > maxx ? lst[4 * n + 0] : maxx;
      miny = lst[4 * n + 1] < miny ? lst[4 * n + 1] : miny;
      maxy = lst[4 * n + 1] > maxy ? lst[4 * n + 1] : maxy;
   }

   if (minx == maxx || miny == maxy)
      return;

   const int minj = (int) floor(minx) - 2;
   const int maxj = (int) ceil(maxx) + 2;
   const int mini = (int) floor(miny) - 2;
   const int maxi = (int) ceil(maxy) + 2;
   const int w    = maxj - minj + 1;
   const int h    = maxi - mini + 1;

   if (isatty(1)) {
      fprintf(stderr, "The (binary) pgm should be sent to a terminal.");
      output_map_header(stdout, w, h);
      fprintf(stderr, "[...] (truncated here)\n");
      return;
   }

#define FROM(num) (mini + (num) * h / THREADS)

   map = malloc(w * h * sizeof(float));

   int num;

   percent = mmap(NULL, THREADS * sizeof(int), PROT_READ | PROT_WRITE,
                  MAP_SHARED | MAP_ANONYMOUS, -1, 0);

   memset(percent, 0, THREADS * sizeof(int));

   for (num = 0; num < THREADS - 1; num++) {
      maps[num] =
         mmap(NULL, (FROM(num + 1) - FROM(num)) * w * sizeof(float),
              PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);

      if (fork() == 0) {
         for (int i = FROM(num); i < FROM(num + 1); i++) {
            const int per =
               100 * (i + 1 - FROM(num)) / (FROM(num + 1) - FROM(num));
            if (percent[num] != per) {
               int count    = 1;
               percent[num] = per;
               msync(percent + num, sizeof(int), MS_ASYNC);
               for (int u = 0; u < THREADS; u++)
                  count += sprintf(buf + count, " %2d%%", percent[u]);
               fwrite(buf, sizeof(char), count, stderr);
            }

            for (int j = minj; j <= maxj; j++) {
               float f = 0.0f;
               for (n = 0; n < nb; n++)
                  f += lst[4 * n + 2] *
                       _pot(lst[4 * n], lst[4 * n + 1], (float) j, (float) i);
               maps[num][(i - FROM(num)) * w + (j - minj)] = f;
            }
         }
         msync(maps[num], (FROM(num + 1) - FROM(num)) * w * sizeof(float),
               MS_SYNC);
         exit(EXIT_SUCCESS);
      }
   }

   for (int i = FROM(num); i < FROM(num + 1); i++) {
      const int per = 100 * (i + 1 - FROM(num)) / (FROM(num + 1) - FROM(num));
      if (percent[num] != per) {
         int count    = 1;
         percent[num] = per;
         msync(percent + num, sizeof(int), MS_ASYNC);
         for (int u = 0; u < THREADS; u++)
            count += sprintf(buf + count, " %2d%%", percent[u]);
         fwrite(buf, sizeof(char), count, stderr);
      }

      for (int j = minj; j <= maxj; j++) {
         float acc = 0.0f;
         for (n = 0; n < nb; n++)
            acc += lst[4 * n + 2] *
                   _pot(lst[4 * n], lst[4 * n + 1], (float) j, (float) i);
         map[(i - mini) * w + (j - minj)] = acc;
      }
   }

   while (wait(NULL) != -1)
      ;
   fprintf(stderr, "\n");
   for (num = 0; num < THREADS - 1; num++) {
      const size_t siz = (FROM(num + 1) - FROM(num)) * w * sizeof(float);
      memcpy(map + (FROM(num) - mini) * w, maps[num], siz);
      munmap(maps[num], siz);
   }

   for (int i = 0; i < w * h; i++)
      if (map[i] <= min_pot)
         min_pot = map[i];

   output_map(stdout, map, w, h, -1.0f / min_pot, 1.0f);
   free(map);
#undef FROM
}

void apply_gravity(const float *lst, const size_t nb, const char **nam)
{
   char buff[512];
   int  num;

   for (num = 0; num < THREADS - 1; num++)
      if (fork() == 0)
         break;

#define FROM(num) (((num) * nb) / THREADS)
   for (size_t i = FROM(num); i < FROM(num + 1); i++) {
#undef FROM
      float  x = lst[4 * i + 0], y = lst[4 * i + 1];
      float  dx = 0.0f, dy = 0.0f;
      size_t n;
      for (size_t j = 0; j < nb; j++)
         if (j != i)
            accum_f(&dx, &dy, lst[4 * j + 0], lst[4 * j + 1], x, y,
                    lst[4 * j + 2]);
      dx *= GRAV_FACT;
      dy *= GRAV_FACT;
      n = snprintf(buff, 511, "%s %f %f\n", nam[i], x + dx, y + dy);
      fwrite(buff, sizeof(char), n, stdout);
      fflush(stdout);
   }

   if (num < THREADS - 1)
      exit(EXIT_SUCCESS);
}

int do_it(const float scale, const bool apply)
{
   char   buf[256];
   float *lst = NULL;
   char **nam = NULL;
   size_t nb  = 0;

   char  *line = NULL;
   size_t n    = 0;
   while (getline(&line, &n, stdin) != -1) {
      if ((nb & (nb + 1)) == 0) {
         lst = realloc(lst, ((nb << 1) | 1) * sizeof(float) * 4);
         if (apply)
            nam = realloc(nam, ((nb << 1) | 1) * sizeof(char *));
      }

      float *const dst = lst + 4 * nb;
      if (3 == sscanf(line, "%255s %f %f", buf, dst, dst + 1)) {
         if (apply)
            nam[nb] = strdup(buf);
         dst[2] = 1.0;
         for (int i = 0; weights[i].nam; i++)
            if (!strcmp(weights[i].nam, buf))
               dst[2] = weights[i].w;
         nb++;
      } else if (line[0] != '\0' && line[0] != '\n') {
         const int n = strlen(line);
         if (line[n - 1] == '\n')
            line[n - 1] = '\0';
         fprintf(stderr, "Ignored line : \"%s\"\n", line);
      }
   }
   // fprintf(stderr,"[%zd systems]\n",nb);
   free(line);

   // HERE
   rad      = _rad * scale / fact;
   mul_ct   = 1.0f / (2.0f * rad * rad * rad);
   add_ct   = -3.0f / (2.0f * rad);
   inv_fact = 1.0f / fact;

   if (apply) {
      apply_gravity(lst, nb, (const char **) nam);
      for (size_t i = 0; i < nb; i++)
         free(nam[i]);
      free(nam);
   } else
      gen_potential(lst, nb, scale);
   free(lst);
   return EXIT_SUCCESS;
}

int usage(char *nam, int ret)
{
   fprintf(stderr, "Usage: %s [ (-s <scale>) | -a ]\n", basename(nam));
   fprintf(stderr, "  Reads a set of node pos in input from standard input.\n");
   fprintf(stderr, "  If -s is set, applies the scaling factor <scale> to "
                   "input, e.g. 0.25\n");
   fprintf(
      stderr,
      "  If -a is set, applies resulting gravity and outputs the result.\n");
   fprintf(stderr, "  If not, outputs a pgm of the gravity potential.\n");
   fprintf(stderr, "  All outputs are sent to standard output.\n");
   return ret;
}

int main(int argc, char **argv)
{
   char *nam   = argv[0];
   bool  apply = false;
   float scale = 1.0f;

   if (argc > 1 && !strcmp(argv[1], "-a")) {
      apply = true;
      argv++;
      argc--;
   }

   if (argc > 1 && !strcmp(argv[1], "-s")) {
      if (apply)
         return usage(nam, EXIT_FAILURE);
      else if (argc < 3 || sscanf(argv[2], "%f", &scale) != 1) {
         fprintf(stderr, "float expected, found \"%s\".\n", argv[2]);
         return usage(nam, EXIT_FAILURE);
      }
      argv += 2;
      argc -= 2;
   }

   if (argc > 1)
      if (!strcmp(argv[1], "-h") || !strcmp(argv[1], "--help"))
         return usage(nam, EXIT_SUCCESS);
      else
         return usage(nam, EXIT_FAILURE);
   else
      return do_it(scale, apply);
}
