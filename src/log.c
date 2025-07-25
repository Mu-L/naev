/*
 * g_
 * See Licensing and Copyright notice in naev.h
 */
/**
 * @file log.c
 *
 * @brief Home of logprintf.
 */
/** @cond */
#include "physfs.h"
#include <stdarg.h>
#include <stdio.h>
#include <time.h> /* strftime */

#include "naev.h"
/** @endcond */

#include "log.h"

#include "conf.h"
#include "debug.h"
#include "ndata.h"

/**< Temporary storage buffers. */
static char *outcopy = NULL;
static char *errcopy = NULL;

static size_t moutcopy; /* Allocated size of outcopy. */
static size_t merrcopy; /* Allocated size of errcopy. */

static int noutcopy = 0; /* Number of bytes written to outcopy. */
static int nerrcopy = 0; /* Number of bytes written to errcopy. */

/**< Output filenames for stdout and stderr. */
static char *outfiledouble = NULL;
static char *errfiledouble = NULL;

/* Whether to copy stdout and stderr to temporary buffers. */
static int copying = 0;

/* File descriptors */
static PHYSFS_File *logout_file = NULL;
static PHYSFS_File *logerr_file = NULL;

/*
 * Prototypes
 */
static int slogprintf( FILE *stream, int newline, const char *str, size_t n );
static int vlogprintf( FILE *stream, int newline, const char *fmt, va_list ap );
static void log_copy( int enable );
static void log_append( const FILE *stream, const char *str );
static void log_cleanStream( PHYSFS_File **file, const char *fname,
                             const char *filedouble );
static void log_purge( void );

static char *_noesc( const char *s, int *ne )
{
   char *buf = calloc( strlen( s ) + 1, sizeof( char ) );
   int   wi = 0, inesc = 0;
   for ( int i = 0; s[i]; i++ )
      if ( inesc ) {
         if ( ( s[i] >= 'a' && s[i] <= 'z' ) || ( s[i] >= 'A' && s[i] <= 'Z' ) )
            inesc = 0;
      } else if ( s[i] == '\e' ) {
         inesc = 1;
      } else
         buf[wi++] = s[i];

   buf[wi] = '\0';
   *ne     = wi;
   return buf;
}

/**
 * @brief va_list version of logprintf and backend.
 */
static int slogprintf( FILE *stream, int newline, const char *str, size_t n )
{
   int   ne;
   char *strne = _noesc( str, &ne );

   /* Append to strfer. */
   if ( copying )
      log_append( stream, str );

   if ( stream == stdout && logout_file != NULL ) {
      PHYSFS_writeBytes( logout_file, strne, newline ? ne + 1 : ne );
      if ( newline )
         PHYSFS_flush( logout_file );
   }

   if ( stream == stderr && logerr_file != NULL ) {
      PHYSFS_writeBytes( logerr_file, strne, newline ? ne + 1 : ne );
      if ( newline )
         PHYSFS_flush( logerr_file );
   }

   /* Also print to the stream. */
   n = fprintf( stream, "%s", str );
   if ( newline )
      fflush( stream );
   free( strne );
   return n;
}

/**
 * @brief va_list version of logprintf and backend.
 */
static int vlogprintf( FILE *stream, int newline, const char *fmt, va_list ap )
{
   va_list aq;
   char   *buf;
   size_t  n;

   /* Offset to add error colour header as necessary. */
   va_copy( aq, ap );
   n = vsnprintf( NULL, 0, fmt, aq );
   va_end( aq );
   buf = malloc( n + 2 );
   n   = vsnprintf( buf, n + 1, fmt, ap );

   /* Finally add newline if necessary. */
   if ( newline ) {
      buf[n]     = '\n';
      buf[n + 1] = '\0';
   } else
      buf[n] = '\0';

   slogprintf( stream, newline, buf, n );
   free( buf );
   return n;
}

/**
 * @brief Like fprintf, but automatically teed to log files (and line-terminated
 * if \p newline is true).
 */
int logprintf( FILE *stream, int newline, const char *fmt, ... )
{
   int     ret;
   va_list ap;
   va_start( ap, fmt );
   ret = vlogprintf( stream, newline, fmt, ap );
   va_end( ap );
   return ret;
}

/**
 * @brief Sets up redirection of stdout and stderr to files.
 * PhysicsFS must be initialized for this to work.
 */
void log_redirect( void )
{
   time_t     cur;
   struct tm *ts;
   char       timestr[20];

   if ( !conf.redirect_file )
      return;

   time( &cur );
   ts = localtime( &cur );
   strftime( timestr, sizeof( timestr ), "%Y-%m-%d_%H-%M-%S", ts );

   PHYSFS_mkdir( "logs" );
   logout_file = PHYSFS_openWrite( "logs/stdout.txt" );
   if ( logout_file == NULL )
      WARN( _( "Unable to redirect stdout to file" ) );

   logerr_file = PHYSFS_openWrite( "logs/stderr.txt" );
   if ( logerr_file == NULL )
      WARN( _( "Unable to redirect stderr to file" ) );

   SDL_asprintf( &outfiledouble, "logs/%s_stdout.txt", timestr );
   SDL_asprintf( &errfiledouble, "logs/%s_stderr.txt", timestr );

   log_copy( 0 );
}

/**
 * @brief Sets up the logging subsystem.
 * (Calling this ensures logging output is preserved until we have a place to
 * save it. That happens after we set up PhysicsFS and call log_redirect().)
 * \see log_copy
 */
void log_init( void )
{
   log_copy( conf.redirect_file );
}

/**
 * @brief Sets up or terminates copying of standard streams into memory.
 *
 * While copying is active, all stdout and stderr-bound messages that pass
 * through logprintf will also be put into a buffer in memory, to be flushed
 * when copying is disabled.
 *
 *    @param enable Whether to enable or disable copying. Disabling flushes
 * logs.
 */
void log_copy( int enable )
{
   /* Nothing to do. */
   if ( copying == enable )
      return;

   if ( enable ) {
      copying = 1;

      moutcopy = 1;
      noutcopy = 0;
      outcopy  = calloc( moutcopy, BUFSIZ );

      merrcopy = 1;
      nerrcopy = 0;
      errcopy  = calloc( merrcopy, BUFSIZ );

      return;
   }

   if ( noutcopy && logout_file != NULL )
      PHYSFS_writeBytes( logout_file, outcopy, strlen( outcopy ) );

   if ( nerrcopy && logerr_file != NULL )
      PHYSFS_writeBytes( logerr_file, errcopy, strlen( errcopy ) );

   log_purge();
}

/**
 * @brief Deletes copied output without printing the contents.
 */
static void log_purge( void )
{
   if ( !copying )
      return;

   free( outcopy );
   free( errcopy );

   outcopy = NULL;
   errcopy = NULL;

   copying = 0;
}

/**
 * @brief Deletes useless (empty) log files from the current session.
 */
void log_clean( void )
{
   log_cleanStream( &logout_file, "logs/stdout.txt", outfiledouble );
   log_cleanStream( &logerr_file, "logs/stderr.txt", errfiledouble );
}

/**
 * @brief \see log_clean
 */
static void log_cleanStream( PHYSFS_File **file, const char *fname,
                             const char *filedouble )
{
   PHYSFS_Stat stat;

   if ( *file == NULL )
      return;

   PHYSFS_close( *file );
   *file = NULL;

   if ( PHYSFS_stat( fname, &stat ) == 0 )
      return;

   if ( stat.filesize == 0 )
      PHYSFS_delete( fname );
   else
      ndata_copyIfExists( fname, filedouble );
}

/**
 * @brief Appends a message to a stream's in-memory buffer.
 *
 *    @param stream Destination stream (stdout or stderr)
 *    @param str String to append.
 */
static void log_append( const FILE *stream, const char *str )
{
   int len = strlen( str );
   if ( stream == stdout ) {
      while ( ( len + noutcopy ) >= (int)moutcopy ) {
         moutcopy *= 2;
         outcopy = realloc( outcopy, moutcopy );
         if ( outcopy == NULL )
            goto copy_err;
      }

      strncpy( &outcopy[noutcopy], str, len + 1 );
      noutcopy += len;
   } else if ( stream == stderr ) {
      while ( ( len + nerrcopy ) >= (int)merrcopy ) {
         merrcopy *= 2;
         errcopy = realloc( errcopy, merrcopy );
         if ( errcopy == NULL )
            goto copy_err;
      }

      strncpy( &errcopy[nerrcopy], str, len + 1 );
      nerrcopy += len;
   }

   return;

copy_err:
   log_purge();
   WARN( _( "An error occurred while buffering %s!" ),
         stream == stdout ? "stdout" : "stderr" );
}

/**
 * @brief Prints warnings, but skips if they are repeated too much.
 */
int log_warn( const char *file, size_t line, const char *func, const char *fmt,
              ... )
{
   static char *warn_last_msg = NULL;
   static int   warn_last_num;
   va_list      ap;
   char        *buf;
   size_t       n;

   /* Create the new message. */
   va_start( ap, fmt );
   n = vsnprintf( NULL, 0, fmt, ap );
   va_end( ap );
   buf = malloc( n + 2 );
   va_start( ap, fmt );
   n = vsnprintf( buf, n + 1, fmt, ap );
   va_end( ap );
   buf[n]     = '\n';
   buf[n + 1] = '\0';

   /* See if we are repeating ourselves. */
   if ( ( warn_last_msg != NULL ) && strcmp( warn_last_msg, buf ) == 0 ) {
      warn_last_num++;
      if ( warn_last_num == 10 )
         logprintf( stderr, 1,
                    _( "LAST WARNING PRINTED %d TIMES, SKIPPING FROM NOW ON" ),
                    10 );
      if ( warn_last_num >= 10 ) {
         free( buf );
         return 0;
      }
   }

   /* First do a backtrace, if possible. */
   debug_logBacktrace();

   /* Display messages. */
   logprintf( stderr, 0, _( "WARNING %s:%lu [%s]: " ), file,
              (unsigned long)line, func );
   slogprintf( stderr, 1, buf, n );

   /* Reset last message. */
   free( warn_last_msg );
   warn_last_msg = buf;

#ifdef DEBUG_PARANOID
#if SDL_PLATFORM_WIN32
   __debugbreak();
#else
   raise( SIGINT );
#endif /* SDL_PLATFORM_WIN32 */
#endif /* DEBUG_PARANOID */
   return n;
}
