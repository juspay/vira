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

### Semantic Colors  
- **Success**: `green-600` (success actions)
- **Destructive**: `red-600` (delete/dangerous actions)
- **Warning**: `yellow-500` (cautionary states)
- **Secondary**: `gray-100` (secondary actions)

### Neutrals
- **Text**: `gray-900` (primary), `gray-600` (secondary), `gray-500` (muted)
- **Borders**: `gray-200` (default), `gray-300` (interactive)
- **Backgrounds**: `gray-50` (sections), `gray-100` (subtle contrast)

## Typography

- **Font**: Inter, system-ui fallbacks
- **Scale**: `text-xs` (12px) → `text-3xl` (30px)
- **Weights**: 400 (normal), 600 (semibold), 700 (bold)

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

## Guidelines

1. **KISS Principle** - Avoid unnecessary abstractions and complexity
2. **Accessibility First** - Follow WCAG contrast requirements
3. **Use existing widgets** - Check `Widgets/*.hs` first
4. **Follow naming** - `vira[ComponentName]_` pattern
5. **Accept attributes** - `[Attributes]` parameter for extensibility
6. **Minimal shadows** - Only use elevation when creating meaningful hierarchy
7. **No gradients** - Prefer solid colors for clarity and performance

---

**Refer to `src/Vira/Widgets/*.hs` for complete component documentation and implementation details.**