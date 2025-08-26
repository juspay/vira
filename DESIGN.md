# Vira Design System

Modern, professional CI/CD application design system emphasizing consistency and accessibility.

## Brand Identity

- **Modern & Professional**: Clean, contemporary design that inspires confidence
- **Glass-morphism**: Subtle gradients and transparency effects
- **Technical but approachable**: Confident without being flashy

## Color Palette

### Primary Colors
- **Indigo**: `#6366f1` (brand), `#4f46e5` (buttons), `#4338ca` (hover)
- **Purple/Blue accents**: `#9333ea`, `#2563eb` (gradients)

### Semantic Colors
- **Success**: Green (`#22c55e`)
- **Error**: Red (`#ef4444`) 
- **Warning**: Yellow (`#eab308`)
- **Info**: Blue (`#3b82f6`)

### Neutrals
Standard gray scale from `#f9fafb` (light) to `#111827` (dark)

## Typography

- **Font**: Inter, system-ui fallbacks
- **Scale**: `text-xs` (12px) → `text-3xl` (30px)
- **Weights**: 400 (normal), 600 (semibold), 700 (bold)

## Components

**All components are defined in `src/Vira/Widgets/*.hs` with full documentation and usage examples.**

### Core Components
- **Buttons**: `viraButton_`, `viraButtonIcon_` (see `Button.hs`)
- **Layout**: `viraCard_`, `viraSection_` (see `Card.hs`, `Layout.hs`)
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

## Layout

- **Responsive**: Mobile-first with `md:`, `lg:`, `xl:` breakpoints
- **Spacing**: Tailwind scale (`space-y-2` to `space-y-8`)
- **Grid**: `grid gap-6 md:grid-cols-2 lg:grid-cols-3`

## Guidelines

1. **Use existing widgets** - Check `Widgets/*.hs` first
2. **Follow naming** - `vira[ComponentName]_` pattern
3. **Accept attributes** - `[Attributes]` parameter for extensibility
4. **Document thoroughly** - Include usage examples and rationale

---

**Refer to `src/Vira/Widgets/*.hs` for complete component documentation and implementation details.**