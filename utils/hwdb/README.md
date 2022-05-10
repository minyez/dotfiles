# Configuration of Hardware database

## Keyboard remaping using udev

See this [post](https://yulistic.gitlab.io/2017/12/linux-keymapping-with-udev-hwdb). Syntax

```plain
KEYBOARD_KEY_<scan_code>=<key_code>
```

`<key_code>` is the key code of the input,
The former can be obtained by checking the system `input-event-codes.h`.
For example, for left control

```c
// keycode in /usr/include/linux/input-event-codes.h
// ...
#define KEY_LEFTCTRL        29
// ...
#define KEY_CAPSLOCK        58
// ...
```

thus the key codes of left control and caps lock are `leftctrl` and `capslock`, respectively.
29 and 58 seem to be the so-called event code.

`<scan_code>` is a value related to specific device.
To find the scan code of a key, one needs to use `evtest`.

Take my Logitech MX Keys keyboard as example.
First, find the input event of the keyboard as follows

```shell
$ cat /proc/bus/input/devices
...
I: Bus=0005 Vendor=046d Product=b35b Version=0011
N: Name="MX Keys Keyboard"
P: Phys=50:2f:9b:29:59:b3
S: Sysfs=/devices/virtual/misc/uhid/0005:046D:B35B.0004/input/input23
U: Uniq=f6:3a:96:1e:22:2b
H: Handlers=sysrq kbd leds event17
B: PROP=0
B: EV=12001f
...
```

This also gives you information about the device: To specify this keyboard, you need to add in the hwdb file

```plain
evdev:input:b0005v046DpB35B*
```

where `0005`, `046D` and `B35B` are the bus, vendor and product number.
Note they must be in uppercase, according to the configuration in the default `60-keyboard.hwdb`
as well as this [comment](https://disq.us/p/26shkb0). The last `*` is used to match the other identifier.

Then, start `evtest` by `sudo evtest /dev/input/event17`, and hit the caps-lock.
You can see output like the following

```plain
Event: time 1630652909.424255, type 4 (EV_MSC), code 4 (MSC_SCAN), value 70039
Event: time 1630652909.424255, type 1 (EV_KEY), code 58 (KEY_CAPSLOCK), value 1
Event: time 1630652909.424255, -------------- SYN_REPORT ------------
Event: time 1630652909.424385, type 17 (EV_LED), code 0 (LED_NUML), value 1
Event: time 1630652909.424385, type 17 (EV_LED), code 1 (LED_CAPSL), value 1
Event: time 1630652909.424385, -------------- SYN_REPORT ------------
Event: time 1630652909.491400, type 4 (EV_MSC), code 4 (MSC_SCAN), value 70039
Event: time 1630652909.491400, type 1 (EV_KEY), code 58 (KEY_CAPSLOCK), value 0
Event: time 1630652909.491400, -------------- SYN_REPORT ------------
```

Thus the scan code of caps-lock key of Logitech MX Keys is `70039` (in hex).

To activate the key remap, copy the hwdb files to `/usr/lib/udev/hwdb.d` and then reload the hwdb service

```bash
sudo cp a.hwdb /usr/lib/udev/hwdb.d/
sudo systemd-hwdb update
sudo udevadm trigger
```

## Available configuration files

Keyboards

- `110-108_MX_Keys.hwdb`: Key remap of Logitech MX Keys

## Some troubleshooting

Having space around "=" may lead to errors like (note the space inside single quotes)
```plain
... Failed to parse scan code from 'KEYBOARD_KEY ' ...
... Failed to parse key code from ' leftctrl' ...
```
In this case just remove the spaces.
