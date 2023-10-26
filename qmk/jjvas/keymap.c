#include QMK_KEYBOARD_H

enum layers { _BASE, _RAISE };

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
	[_BASE] = LAYOUT_ortho_4x12( \
		KC_LGUI, KC_Q   , KC_W   , KC_E   , KC_R   , KC_T   , KC_Y   , KC_U   , KC_I   , KC_O   , KC_P   , KC_MINS, \
		KC_LCTL, KC_A   , KC_S   , KC_D   , KC_F   , KC_G   , KC_H   , KC_J   , KC_K   , KC_L   , KC_SCLN, KC_QUOT, \
		KC_LALT, KC_Z   , KC_X   , KC_C   , KC_V   , KC_B   , KC_N   , KC_M   , KC_COMM, KC_DOT , KC_SLSH, KC_EQL , \
		KC_HOME, KC_PGDN, KC_PGUP, KC_END , MO(1)  , KC_SPC , KC_RALT, KC_LSFT, KC_LEFT, KC_DOWN, KC_UP  , KC_RGHT  \
	),

	[_RAISE] = LAYOUT_ortho_4x12( \
		KC_F12 , KC_F1  , KC_F2  , KC_F3  , KC_F4  , KC_F5  , KC_F6  , KC_F7  , KC_F8  , KC_F9  , KC_F10 , KC_F11 , \
		_______, KC_DEL , KC_BSPC, KC_TAB , KC_ENT , KC_NUBS, KC_NUHS, KC_LBRC, KC_RBRC, KC_BSLS, KC_GRV , KC_CAPS, \
		_______, KC_1   , KC_2   , KC_3   , KC_4   , KC_5   , KC_6   , KC_7   , KC_8   , KC_9   , KC_0   , KC_APP , \
		KC_MUTE, KC_VOLD, KC_VOLU, KC_MPLY, _______, KC_INS , KC_ESC , _______, KC_MPRV, KC_BRID, KC_BRIU, KC_MNXT  \
	),
};
