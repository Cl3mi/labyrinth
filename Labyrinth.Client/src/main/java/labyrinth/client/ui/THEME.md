Theme & Styling Guide

Purpose
- Centralize colors and fonts so all UI components use the same visual language.
- Use the helpers in `labyrinth.client.ui.theme` instead of inlining colors / fonts.

Main APIs
- GameTheme
  - `GameTheme.Colors` — static color palette and helper getters that proxy to `ThemeManager`.
  - `GameTheme.Spacing`, `GameTheme.Effects` — layout & effect constants.

- ThemeManager
  - runtime theme state (dark/light) and dynamic getters like `getSurfacePrimary()`, `getShadow()`, `getBackgroundImagePath()`.

- FontManager
  - loads custom fonts and exposes presets like `getLargeUI()`, `getMediumDisplay()`, `getSmallUIBold()`.

- ThemeEffects
  - small helpers for common effects: `withAlpha`, `brighten`, `blendColors`, and a few drawing helpers.

How to add styles in code
- Colors: use `GameTheme.Colors` (e.g. `GameTheme.Colors.ACCENT_GOLD`) or dynamic `ThemeManager.getInstance().getSurfacePrimary()`.
- Fonts: use `FontManager` presets (`getMediumUI()`, `getLargeDisplay()`, `getSmallUIBold()`).
- Opacity or small adjustments: use `ThemeEffects.withAlpha(color, alpha)` or `ThemeEffects.brighten(color, amount)`.

Examples
- Label: `label.setFont(FontManager.getMediumUI()); label.setForeground(ThemeManager.getInstance().getTextPrimary());`
- Custom painting: `g2.setColor(ThemeEffects.withAlpha(GameTheme.Colors.shadow(), 80));`

Notes
- Theme constants in `ThemeManager` are the source of truth for dark/light palettes. If you need a new color, add it to `GameTheme.Colors` and wire it into `ThemeManager`.
- Avoid using HTML inline color styles in labels (e.g. `<span style='color:#fff'>`); prefer setting foreground colors on JLabel or using icons.

If you want, I can also add a small run-time 'theme switcher' debug menu that prints current colors to console (useful for visual tuning).
