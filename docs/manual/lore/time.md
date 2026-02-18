# Universal Synchronized Time (UST)

Universal Synchronized Time (UST) is the standard time system in Naev.

## Explanation

UST consists of three basic components describing different amounts of time: the standard time unit (stu), which is equivalent to an Earth second; the period, which is equal to 10,000 stu (or earth seconds); and the cycle, which is equal to 5,000 periods (50,000,000 stu).
In colloquial usage, the terms "decaperiod" (equivalent to 10 periods) and "hectostu" (equivalent to 100 stu) are also common.

* UST C:PPPP.SSSS

Where "C" is the cycle, "PPPP" is the period (always displayed as four digits), and "SSSS" is the stu (always displayed as four digits).
So for example, the following hypothetical date indicates cycle 493, period 42, stu 2089 (which is about 100 cycles prior to [[The Incident]]):

* UST 493:0042.2089

When describing lengths of time, it is commonplace for computer systems to indicate a number of periods with either "p" or "stp", and a number of standard time units with "stu".
This convention is not used for cycles, which are always spelled out in full as "cycles", but is also partially adopted in spoken form where periods are just referred as "periods", however, earth seconds are referred to as "stu" (pronounced like stew).
This also happens to solve the confusion between real-time seconds, and in-game earth seconds or stu.
Additionally, due to the metric nature of the time system, periods and stu can be written out as a single unit, although in spoken conversation people report the periods and cycles separately.
Some examples:

* 783p (read as "783 periods")
* 42 stu (read as "42 stu")
* 12.0456 p (read as "12 periods and 456 stu")

The following is a chart of all time units used in Naev along with the corresponding Earth time unit they are similar to in terms of where they are used.

| UST unit     | Abbreviation | Length of Time | Equal to (in Earth time) | Used like |
|:-------------|:-------------|:---------------|:-------------------------|:----------|
| Standard Time Unit | "stu"          | 1 Earth second | 1 second                 | Seconds   |
| Hectostu     | N/A          | 100 stu | 1 minute and 40 seconds  | Minutes   |
| Periods      | "p"          | 10,000 stu | ~2 hours and 47 minutes  | Hours     |
| Decaperiods  | N/A          | 10 periods     | ~28 hours                | Days      |
| Cycles       | N/A          | 5,000 periods  | ~579 days                | Years     |

## Time passage

Following is a list of actions and how much time they take in Naev.

* **Flying in space**: For ships with a time dilation rate of 100% (that is, most small ships), time passes at a rate of 30 stu per real-world second, which is why the GUI's clock increases by 0.01p every 3⅓ seconds. For ships with higher time dilation the passage of time is faster, and for ships with lower time dilation the passage of time is slower.
* **Landed**: Time does not pass while landed.
* **Takeoff**: Taking off takes 1 period, which means that stopping to refuel during time-sensitive missions is generally a bad idea.
* **Jumping**: Hyperspace jumps also take time, generally 1 period per jump, though some ships such as the Quicksilver take less.
