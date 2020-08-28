/* Copyright 2019
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

enum layers {
  _QWERTY = 0,
  _RAISE,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[_QWERTY] = LAYOUT_ortho_4x12( \
  KC_LGUI, KC_Q   , KC_W   , KC_E   , KC_R   , KC_T   , KC_Y   , KC_U   , KC_I   , KC_O   , KC_P   , KC_MINS, \
  KC_LCTL, KC_A   , KC_S   , KC_D   , KC_F   , KC_G   , KC_H   , KC_J   , KC_K   , KC_L   , KC_SCLN, KC_QUOT, \
  KC_LALT, KC_Z   , KC_X   , KC_C   , KC_V   , KC_B   , KC_N   , KC_M   , KC_COMM, KC_DOT , KC_SLSH, KC_EQL , \
  MO(1)  , KC_DEL , KC_INS , KC_ESC , KC_RALT, KC_SPC , KC_LSFT, KC_ENT , KC_LEFT, KC_DOWN, KC_UP  , KC_RIGHT \
),

[_RAISE] = LAYOUT_ortho_4x12( \
  _______, KC_F9  , KC_F10 , KC_F11 , KC_F12 , KC_VOLD, KC_VOLU, KC_MUTE, KC_7   , KC_8   , KC_9   , KC_GRV , \
  _______, KC_F5  , KC_F6  , KC_F7  , KC_F8  , KC_BRID, KC_BRIU, KC_APP , KC_5   , KC_6   , KC_7   , KC_BSLS, \
  _______, KC_F1  , KC_F2  , KC_F3  , KC_F4  , KC_KANA, KC_LBRC, KC_RBRC, KC_1   , KC_2   , KC_3   , KC_0   , \
  _______, KC_PAUS, KC_PSCR, KC_SLCK, KC_CAPS, KC_TAB , _______, KC_BSPC, KC_HOME, KC_PGDN, KC_PGUP, KC_END   \
),

};