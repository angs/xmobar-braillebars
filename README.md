cpubar
=======================
Multicore CPU utilization bar for xmobar using braille patterns.
Works with any number of cores.

To be used in xmobar with
  Run CommandReader "cpubar" "cpubar"


netbar
=======================
Logarithmic net usage bar for xmobar using braille patterns
xmobar usage example:
  Run CommandReader "netbar eth0" "netbar"


anybar
=======================
Visualize any value as a vertical bar in xmobar with braille patterns

Example: show volume:

  Run CommandReader "bash -c 'while true; do amixer -M -c 1 get Master | tail -n 1 | grep -o [0-9]*% | xargs -n1 anybar -t ♫ -l 8; sleep 0.5; done'" "volume"

