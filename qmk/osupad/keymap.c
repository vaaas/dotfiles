#include QMK_KEYBOARD_H
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
   [0] = LAYOUT_ortho_4x4(
      KC_LEFT, KC_DOWN, KC_UP, KC_RGHT,
      KC_LEFT, KC_DOWN, KC_UP, KC_RGHT,
      KC_LEFT, KC_DOWN, KC_UP, KC_RGHT,
      KC_LEFT, KC_DOWN, KC_UP, KC_RGHT
   ),
};
