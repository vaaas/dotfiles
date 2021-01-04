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

#define SFT_TAB LSFT_T(KC_TAB)
#define L1_APP LT(1, KC_APP)
#define GUI_ESC LGUI_T(KC_ESC)
#define CLT_MIN LCTL_T(KC_MINS)
#define ALT_EQL LALT_T(KC_EQL)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[_BASE] = LAYOUT_ortho_5x15(
KC_NUHS, KC_1   , KC_2   , KC_3   , KC_4   , KC_5   , KC_6   , KC_7   , KC_8   , KC_9   , KC_0   , KC_NUBS, KC_F13 , KC_F14 , KC_F15 , \
GUI_ESC, KC_K   , KC_C   , KC_D   , KC_M   , KC_J   , KC_Q   , KC_W   , KC_U   , KC_V   , KC_SCLN, KC_GRV , KC_F10 , KC_F11 , KC_F12 , \
CTL_MIN, KC_R   , KC_S   , KC_T   , KC_N   , KC_H   , KC_Y   , KC_I   , KC_E   , KC_O   , KC_A   , KC_QUOT, KC_F7  , KC_F8  , KC_F9  , \
ALT_EQL, KC_Z   , KC_F   , KC_G   , KC_L   , KC_X   , KC_B   , KC_P   , KC_COMM, KC_DOT , KC_SLSH, KC_BSLS, KC_F4  , KC_F5  , KC_F6  , \
KC_HOME, KC_PGDN, KC_PGUP, KC_END , L1_APP , KC_SPC , KC_ENT , SFT_TAB, KC_LEFT, KC_DOWN, KC_UP  , KC_RGHT, KC_F1  , KC_F2  , KC_F3    \
),
[_RAISE] = LAYOUT_ortho_5x15(
_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, \
_______, _______, _______, _______, _____ _, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, \
_______, KC_RGUI, KC_RCTL, KC_RALT, KC_RSFT, KC_P4  , KC_P5  , KC_LBRC, KC_RBRC, KC_P1  , KC_P2  , KC_P3  , _______, _______, _______, \
_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, \
KC_INS , KC_MUTE, KC_VOLD, KC_VOLU, _______, KC_DEL , KC_BSPC, _______, KC_PSCR, KC_CAPS, KC_NLCK, KC_SLC , _______, _______, _______  \
)
};
