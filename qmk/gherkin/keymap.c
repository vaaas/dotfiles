#include QMK_KEYBOARD_H

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
	[0] = LAYOUT_ortho_3x10( // base layer
		KC_K,    KC_C,    KC_D,    KC_H,    KC_RALT, KC_LGUI, KC_W,    KC_U,    KC_O,    C_V,
		KC_A,    KC_S,    KC_D,    KC_F,    KC_LCTL, KC_LALT, KC_I,    KC_E,    KC_A,    KC_L,
		KC_MUTE, KC_VOLD, KC_VOLU, MO(2),   KC_SPC,  KC_LSFT, MO(1),   KC_BTN1, KC_BTN2, KC_BTN3
	),

	[1] = LAYOUT_ortho_3x10( // alpha 2 layer
		KC_Q,    KC_X,    KC_Z,    KC_B,    _______, _______, KC_QUOT, KC_MINS, KC_BSLS, KC_GRV,
		KC_P,    KC_F,    KC_G,    KC_M,    _______, _______, KC_Y,    KC_J,    KC_COMM, KC_DOT,
		_______, _______, _______, MO(4),   _______, _______, _______, _______, _______, _______
	),

	[2] = LAYOUT_ortho_3x10( // sys layer
		KC_APP,  KC_CAPS, KC_ESC,  KC_INS,  _______, _______, KC_HOME, KC_PGDN, KC_PGUP, KC_END,
		KC_DEL,  KC_BSPC, KC_TAB,  KC_ENT,  _______, _______, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT,
		_______, _______, _______, _______, _______, MO(3),   _______, _______, _______, QK_BOOT
	),

	[3] = LAYOUT_ortho_3x10( // num layer
		KC_1,    KC_2,    KC_3,    KC_4,    _______, _______, KC_9,    KC_8,    KC_7,    KC_6,
		KC_SLSH, KC_EQL,  KC_MINS, KC_5,    _______, _______, KC_0,    KC_LBRC, KC_RBRC, KC_BSLS,
		_______, _______, _______, _______, _______, _______, _______, _______, _______, _______
	),

	[4] = LAYOUT_ortho_3x10( // fkeys/etc layer
		KC_F1,   KC_F2,   KC_F3,   KC_F4,   _______, _______, KC_F9,   KC_F10,  KC_F11,  KC_F12,
		KC_F5,   KC_F6,   KC_F7,   KC_F8,   _______, _______, KC_F13,  KC_F14,  KC_F15,  KC_F16,
		_______, _______, _______, _______, _______, _______, _______, _______, _______, _______
	)
};

void keyboard_pre_init_user(void) {
	// Call the keyboard pre init code.
	// Set our LED pins as output
	setPinOutput(D5);
	setPinOutput(B0);
}

void led_set_user(uint8_t usb_led) {
	if (IS_LED_ON(usb_led, USB_LED_NUM_LOCK))
		writePinLow(D5);
	else
		writePinHigh(D5);

	if (IS_LED_ON(usb_led, USB_LED_CAPS_LOCK))
		writePinLow(B0);
	else
		writePinHigh(B0);
}
