/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 4;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "Nintendo DS BIOS Vasified:pixelsize=16" };
static const char col_accnt[]       = "#bb4444";
static const char col_text[]        = "#444444";
static const char col_main[]        = "#ffeedd";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_text, col_main,  col_main },
	[SchemeSel]  = { col_main, col_accnt, col_accnt },
};

/* tagging */
static const char *tags[] = { "wrk", "www", "cmd", "fun", "etc" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ NULL,       NULL,       NULL,       0,            False,       -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "T",      tile },    /* first entry is default */
	//{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "M",      monocle },
	{ "C",      centeredmaster },
	{ "CF",      centeredfloatingmaster },
	{ NULL,       NULL }
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|Mod1Mask,              KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      toggletag,      {.ui = 1 << TAG} },

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_e,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_o,      focusstack,     {.i = -1 } },
	{ MODKEY|ControlMask,           XK_e,      cyclelayout,    {.i = +1 } },
	{ MODKEY|ControlMask,           XK_o,      cyclelayout,    {.i = -1 } },
	{ MODKEY,                       XK_i,      view_adjacent,  {.i = -1 } },
	{ MODKEY,                       XK_a,      view_adjacent,  {.i = +1 } },
	{ MODKEY|ControlMask,           XK_i,      setmfact,       {.f = -0.125} },
	{ MODKEY|ControlMask,           XK_a,      setmfact,       {.f = +0.125} },
	{ MODKEY|Mod1Mask,              XK_i,      setmfact,       {.f = -0.015625} },
	{ MODKEY|Mod1Mask,              XK_a,      setmfact,       {.f = +0.015625} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                   XK_apostrophe, view,           {0} },
	{ MODKEY,                       XK_q,      killclient,     {0} },
	{ MODKEY|ControlMask,           XK_f,      togglefloating, {0} },
	TAGKEYS(                        XK_w,                      0)
	TAGKEYS(                        XK_u,                      1)
	TAGKEYS(                        XK_v,                      2)
	TAGKEYS(                        XK_semicolon,              3)
	TAGKEYS(                        XK_grave,                  4)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
