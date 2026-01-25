# Vira Design System

Clean, minimal CI/CD application design system emphasizing clarity and functionality.

## Design Principles

- **KISS (Keep It Simple)**: Minimal, focused design without unnecessary decoration
- **Flat Design**: Clean layouts with selective elevation for hierarchy
- **Technical Clarity**: Clear visual communication over visual flourishes

## Color Palette

### Primary Colors

- **Indigo**: `indigo-600` (primary actions), `indigo-700` (hover states)
- **Background**: `gray-50` (page), `white` (cards), `gray-200` (dividers)
  - **Dark mode**: `gray-900` (page), `gray-800` (cards), `gray-700` (dividers)

### Semantic Colors

- **Success**: `green-600` (success actions)
- **Destructive**: `red-600` (delete/dangerous actions)
- **Warning**: `yellow-500` (cautionary states)
- **Secondary**: `gray-100` (secondary actions)
  - **Dark mode**: `gray-700` (secondary actions)

### Neutrals

- **Text**: `gray-900` (primary), `gray-600` (secondary), `gray-500` (muted)
  - **Dark mode**: `gray-100` (primary), `gray-300` (secondary), `gray-400` (muted)
- **Borders**: `gray-200` (default), `gray-300` (interactive)
  - **Dark mode**: `gray-700` (default), `gray-600` (interactive)
- **Backgrounds**: `gray-50` (sections), `gray-100` (subtle contrast)
  - **Dark mode**: `gray-800` (sections), `gray-700` (subtle contrast)

## Typography

### Font Families

- **Sans-serif**: Inter (variable font, 300-700 weights), system-ui fallbacks
- **Monospace**: JetBrains Mono (for logs, code, and technical values)

### Size Scale

| Class       | Size | Usage                                                |
| ----------- | ---- | ---------------------------------------------------- |
| `text-xs`   | 12px | Badges, table headers (uppercase), tertiary metadata |
| `text-sm`   | 14px | Body text, form labels, buttons, timestamps          |
| `text-base` | 16px | Breadcrumbs, emphasized body content                 |
| `text-lg`   | 18px | Card subheadings, section descriptions               |
| `text-xl`   | 20px | Section headers (h2, h3)                             |
| `text-2xl`  | 24px | Page headers (h1)                                    |

### Weight Guidelines

| Weight | Class           | Usage                                     |
| ------ | --------------- | ----------------------------------------- |
| 500    | `font-medium`   | Badges, subtle emphasis, secondary labels |
| 600    | `font-semibold` | Buttons, tab labels, highlighted text     |
| 700    | `font-bold`     | All headings (h1-h3), important values    |

### Patterns

```haskell
-- Page headers (h1)
h1_ [class_ "text-2xl font-bold text-gray-900 dark:text-gray-100"]

-- Section headers (h2)
h2_ [class_ "text-xl font-bold text-gray-900 dark:text-gray-100 mb-4"]

-- Buttons
button_ [class_ "text-sm font-semibold"]

-- Badges
span_ [class_ "text-xs font-medium"]

-- Table headers
th_ [class_ "text-xs font-medium uppercase tracking-wider"]
```

## Spacing

### Scale

| Class | Size | Common Usage                       |
| ----- | ---- | ---------------------------------- |
| `1`   | 4px  | Icon margins, tight gaps           |
| `2`   | 8px  | Badge padding, inline spacing      |
| `3`   | 12px | Button padding (vertical)          |
| `4`   | 16px | Card content margins, section gaps |
| `6`   | 24px | Section spacing, card padding      |
| `8`   | 32px | Page section dividers              |

### Component Patterns

| Component         | Pattern                                                   |
| ----------------- | --------------------------------------------------------- |
| **Cards**         | `p-6` (content), `mb-6` (between cards)                   |
| **Buttons**       | `px-6 py-3` (primary), `px-4 py-2` (secondary)            |
| **Badges**        | `px-2 py-1` or `px-3 py-1`                                |
| **Form groups**   | `space-y-6` (between groups), `mb-2` (label to input)     |
| **Page sections** | `space-y-6` or `mb-8`                                     |
| **Inline items**  | `space-x-2` (tight), `space-x-4` (normal), `gap-4` (flex) |

### Border Radius

| Class          | Usage                                |
| -------------- | ------------------------------------ |
| `rounded`      | Small elements, code blocks          |
| `rounded-lg`   | **Default** - buttons, cards, inputs |
| `rounded-full` | Badges, pills, avatars               |
| `rounded-xl`   | Large feature cards, modals          |

## Components

**All components are defined in `src/Vira/Widgets/*.hs` with full documentation and usage examples.**

### Core Components

- **Buttons**: `viraButton_`, `viraButtonIcon_` (see `Button.hs`)
- **Cards**: `viraCard_` (flat), `viraCardElevated_` (with shadow) (see `Card.hs`)
- **Layout**: `viraSection_`, `viraPageHeader_`, `viraDivider_` (see `Layout.hs`)
- **Forms**: `viraInput_`, `viraLabel_` (see `Form.hs`)
- **Status**: `viraStatusBadge_` (see `Status.hs`)
- **Alerts**: `viraAlert_` (see `Alert.hs`)

### Usage

```haskell
import Vira.Widgets qualified as W

W.viraButton_ W.ButtonPrimary [] $ do
  W.viraButtonIcon_ $ toHtmlRaw Icon.plus
  "Add Repository"
```

## Icons

- **Library**: Tabler Icons (`Web.TablerIcons.Outline`)
- **Standard sizes**: `w-4 h-4` (buttons), `w-5 h-5` (UI), `w-6 h-6` (headers), `w-8 h-8` (sections), `w-16 h-16` (hero)
- **Centering**: Always use `flex items-center justify-center`

## Visual Hierarchy

- **Elevation**: Flat by default, `shadow-sm` only for important content
- **Spacing**: Consistent spacing scale using Tailwind (`space-y-*`)
- **Typography**: Clear text hierarchy with `text-3xl`, `text-lg`, etc.
- **Color**: Semantic colors communicate meaning, not decoration

## Layout

- **Responsive**: Mobile-first with `md:`, `lg:`, `xl:` breakpoints
- **Grid**: `grid gap-6 md:grid-cols-2 lg:grid-cols-3`
- **Container**: `container mx-auto` with responsive padding

## Accessibility Guidelines

### Color Contrast

- **Primary text**: `text-gray-900` (high contrast)
- **Secondary text**: `text-gray-600` (medium contrast)
- **Muted text**: `text-gray-500` (minimum contrast)
- **Avoid**: `text-gray-400` (insufficient contrast)

### Interactive Elements

- **Focus states**: Clear ring indicators for keyboard navigation
- **Button text**: High contrast white on colored backgrounds
- **Icon colors**: Use `text-gray-500` minimum for functional icons

### Small Text Guidelines

- **Extra small text** (`text-xs`): Use `text-gray-600` minimum
- **Regular text**: Can use `text-gray-500` for secondary information
- **Interactive labels**: Always use `text-gray-600` or darker

## Dark Mode

Vira implements automatic dark mode using Tailwind's `@media (prefers-color-scheme: dark)` strategy. Dark mode activates based on system preferences without any JavaScript or configuration.

### Dark Mode Guidelines

1. **Always add dark variants** - When using gray colors, always include `dark:` variants
2. **Use semantic opacity** - Use opacity for tinted backgrounds (e.g., `dark:bg-red-900/20`)
3. **Maintain contrast** - Ensure text remains readable in both modes
4. **Test both modes** - Always verify styling in light and dark modes

### Common Dark Mode Patterns

```haskell
-- Backgrounds
"bg-white dark:bg-gray-800"
"bg-gray-50 dark:bg-gray-900"
"bg-gray-100 dark:bg-gray-700"

-- Borders
"border-gray-200 dark:border-gray-700"
"border-gray-300 dark:border-gray-600"

-- Text
"text-gray-900 dark:text-gray-100"
"text-gray-600 dark:text-gray-300"
"text-gray-500 dark:text-gray-400"

-- Semantic colors with opacity
"bg-red-50 dark:bg-red-900/20"
"bg-blue-50 dark:bg-blue-900/20"
```

## Guidelines

1. **KISS Principle** - Avoid unnecessary abstractions and complexity
2. **Accessibility First** - Follow WCAG contrast requirements
3. **Dark Mode** - Always include `dark:` variants for gray colors
4. **Use existing widgets** - Check `Widgets/*.hs` first
5. **Follow naming** - `vira[ComponentName]_` pattern
6. **Accept attributes** - `[Attributes]` parameter for extensibility
7. **Minimal shadows** - Only use elevation when creating meaningful hierarchy
8. **No gradients** - Prefer solid colors for clarity and performance

---

**Refer to `src/Vira/Widgets/*.hs` for complete component documentation and implementation details.**
