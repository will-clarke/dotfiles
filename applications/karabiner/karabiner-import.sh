#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set repeat.initial_wait 300
/bin/echo -n .
$cli set remap.uk_section2hash 1
/bin/echo -n .
$cli set remap.programmer.backtick_underscore 1
/bin/echo -n .
$cli set option.emacsmode_controlM 1
/bin/echo -n .
$cli set option.emacsmode_controlH 1
/bin/echo -n .
$cli set repeat.wait 10
/bin/echo -n .
$cli set remap.controlL2controlL_escape 1
/bin/echo -n .
$cli set remap.controlJ2return 1
/bin/echo -n .
$cli set remap.mouse_keys_mode_2 1
/bin/echo -n .
$cli set remap.pc_application2optionR 1
/bin/echo -n .
$cli set remap.fnletter_to_ctrlletter 1
/bin/echo -n .
/bin/echo
