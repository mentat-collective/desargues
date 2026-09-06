# Self-hosted faces

Montserrat 700 and Open Sans (variable, 300–800, upright and italic), latin
subset, as served by Google Fonts on 2026-09-06. Both are licensed under the
SIL Open Font License 1.1. They are here so the page does not chain a
render-blocking request through fonts.googleapis.com on every visit; the
`@font-face` rules live at the top of css/plato.css, and `bb assets` strips the
Reveal theme's `@import` of the same families.
