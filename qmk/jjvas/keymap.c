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
  _BASE,
  _RAISE,
};

#define LT_APP LT(1, KC_APP)
#define LT_ENT LT(1, KC_ENT)
#define CTL_KN LCTL_T(KC_N)
#define CTL_KI RCTL_T(KC_I)
#define SFT_KT LSFT_T(KC_T)
#define SFT_KE RSFT_T(KC_E)
#define ALT_KS LALT_T(KC_S)
#define ALT_KO LALT_T(KC_O)
#define GUI_KR LGUI_T(KC_R)
#define GUI_KA RGUI_T(KC_A)
#define ALT_SPC RALT_T(KC_SPC)
#define ALT_TAB RALT_T(KC_TAB)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[_BASE] = LAYOUT_ortho_4x12( \
KC_NUHS, KC_X   , KC_C   , KC_D   , KC_M   , KC_J   , KC_Q   , KC_W   , KC_U   , KC_V   , KC_SCLN, KC_GRV , \
KC_MINS, GUI_KR , ALT_KS , SFT_KT , CTL_KN , KC_H   , KC_Y   , CTL_KI , SFT_KE , ALT_KO , GUI_KA , KC_QUOT, \
KC_EQL , KC_Z   , KC_F   , KC_G   , KC_L   , KC_K   , KC_B   , KC_P   , KC_COMM, KC_DOT , KC_SLSH, KC_BSLS, \
KC_HOME, KC_PGDN, KC_PGUP, KC_END , LT_APP , ALT_SPC, ALT_TAB, LT_ENT , KC_LEFT, KC_DOWN, KC_UP  , KC_RGHT  \
),
[_RAISE] = LAYOUT_ortho_4x12( \
KC_F11 , KC_F1  , KC_F2  , KC_F3  , KC_F4  , KC_F5  , KC_F6  , KC_F7  , KC_F8  , KC_F9  , KC_F10 , KC_F12 , \
KC_LBRC, KC_1   , KC_2   , KC_3   , KC_4   , KC_5   , KC_6   , KC_7   , KC_8   , KC_9   , KC_0   , KC_RBRC, \
KC_CAPS, KC_P1  , KC_P2  , KC_P3  , KC_P4  , KC_P5  , KC_P6  , KC_P7  , KC_P8  , KC_P9  , KC_P0  , KC_NLCK, \
KC_MUTE, KC_VOLD, KC_VOLU, KC_MPLY, KC_ESC , KC_INS , KC_DEL , KC_BSPC, KC_MPRV, KC_BRID, KC_BRIU, KC_MNXT  \
),
};
