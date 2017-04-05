#!/bin/bash
# See https://wiki.archlinuxjp.org/index.php/Synaptics_%E3%82%BF%E3%83%83%E3%83%81%E3%83%91%E3%83%83%E3%83%89#.E3.82.BD.E3.83.95.E3.83.88.E3.82.A6.E3.82.A7.E3.82.A2.E3.83.88.E3.82.B0.E3.83.AB
synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')
