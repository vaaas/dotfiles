/* Copyright 2017 Wunder
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H

enum layer_names {
	_BASE,
	_RAISE
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[_BASE] = LAYOUT_ortho_5x15(
KC_LBRC, KC_1   , KC_2   , KC_3   , KC_4   , KC_5   , KC_6   , KC_7   , KC_8   , KC_9   , KC_0   , KC_RBRC, KC_NLCK, KC_CAPS, KC_SLCK,
KC_LGUI, KC_K   , KC_C   , KC_D   , KC_M   , KC_J   , KC_Q   , KC_W   , KC_U   , KC_V   , KC_SCLN, KC_GRV , KC_F10 , KC_F11 , KC_F12 ,
KC_LCTL, KC_R   , KC_S   , KC_T   , KC_N   , KC_H   , KC_Y   , KC_I   , KC_E   , KC_O   , KC_A   , KC_QUOT, KC_F7  , KC_F8  , KC_F9  ,
KC_LALT, KC_Z   , KC_F   , KC_G   , KC_L   , KC_X   , KC_B   , KC_P   , KC_COMM, KC_DOT , KC_SLSH, KC_BSLS, KC_F4  , KC_F5  , KC_F6  ,
KC_ESC , KC_DEL , KC_INS , KC_APP , MO(1) ,  KC_SPC,  KC_RALT, KC_LSFT, KC_LEFT, KC_DOWN, KC_UP  , KC_RGHT, KC_F1  , KC_F2  , KC_F3
),
[_RAISE] = LAYOUT_ortho_5x15(
KC_HELP, KC_P1  , KC_P2  , KC_P3  , KC_P4  , KC_P5  , KC_P6  , KC_P7  , KC_P8  , KC_P9  , KC_P0  , KC_PENT, KC_PAUS, KC_PEQL, KC_PSCR,
_______, KC_LANG1,KC_LANG2,KC_LANG3,KC_LANG4,KC_LANG5,KC_LANG6,KC_LANG7,KC_LANG8,KC_LANG9,KC_PDOT, KC_PCMM, KC_F22 , KC_F23 , KC_F24 ,
_______, KC_MINS, KC_EQL , KC_TAB , KC_ENT , KC_NUHS, KC_NUBS, KC_RGUI, KC_RCTL, KC_RSFT, KC_PMNS, KC_PPLS, KC_F19 , KC_F20 , KC_F21 ,
_______, KC_INT1, KC_INT2, KC_INT3, KC_INT4, KC_INT5, KC_INT6, KC_INT7, KC_INT8, KC_INT9, KC_PSLS, KC_PAST, KC_F16 , KC_F17 , KC_F18 ,
KC_CANCEL, KC_MUTE, KC_VOLD, KC_VOLU, _______, KC_EXEC, KC_BSPC, _______, KC_HOME, KC_PGUP, KC_PGDN, KC_END , KC_F13 , KC_F14 , KC_F15
)
};
