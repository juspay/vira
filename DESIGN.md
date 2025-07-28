# Vira Design System

This document defines the official design system and brand guidelines for the Vira CI/CD application. Follow these guidelines to maintain visual consistency and a professional appearance across all UI components.

## 🎨 Brand Identity

### Core Principles
- **Modern & Professional**: Clean, contemporary design that inspires confidence
- **Clarity & Focus**: Clear information hierarchy with minimal cognitive load
- **Consistency**: Unified visual language across all interfaces
- **Accessibility**: Inclusive design that works for all users

### Visual Style
- **Aesthetic**: Modern glass-morphism with subtle gradients
- **Personality**: Professional, reliable, efficient, innovative
- **Tone**: Technical but approachable, confident without being flashy

## 🌈 Color Palette

### Primary Colors
```css
/* Primary Brand Colors - Use for main actions and branding */
--primary-50:  #eef2ff   /* Very light indigo backgrounds */
--primary-100: #e0e7ff   /* Light indigo backgrounds */
--primary-500: #6366f1   /* Primary indigo - main brand color */
--primary-600: #4f46e5   /* Primary button color */
--primary-700: #4338ca   /* Primary button hover */
--primary-900: #312e81   /* Dark indigo text */

/* Secondary Purple Accent */
--purple-600: #9333ea    /* Used in gradients */
--blue-600:   #2563eb    /* Used in gradients */
```

### Semantic Colors
```css
/* Success - Green */
--success-50:  #f0fdf4
--success-100: #dcfce7
--success-500: #22c55e
--success-700: #15803d
--success-800: #166534

/* Error - Red */
--error-50:   #fef2f2
--error-100:  #fee2e2
--error-500:  #ef4444
--error-700:  #dc2626
--error-800:  #991b1b

/* Warning - Yellow */
--warning-50:  #fefce8
--warning-100: #fef3c7
--warning-500: #eab308
--warning-700: #a16207
--warning-800: #854d0e

/* Info - Blue */
--info-50:   #eff6ff
--info-100:  #dbeafe
--info-500:  #3b82f6
--info-700:  #1d4ed8
--info-800:  #1e40af
```

### Neutral Colors
```css
/* Grays - For text, borders, backgrounds */
--gray-50:   #f9fafb   /* Light backgrounds */
--gray-100:  #f3f4f6   /* Card backgrounds */
--gray-200:  #e5e7eb   /* Borders */
--gray-300:  #d1d5db   /* Disabled states */
--gray-500:  #6b7280   /* Secondary text */
--gray-600:  #4b5563   /* Primary text on light */
--gray-700:  #374151   /* Headings */
--gray-800:  #1f2937   /* Dark text */
--gray-900:  #111827   /* Darkest text */

/* White/Transparent */
--white:     #ffffff
--slate-50:  #f8fafc   /* Background gradient start */
--blue-50:   #eff6ff   /* Background gradient end */
```

## 📝 Typography

### Font Family
```css
font-family: 'Inter', ui-sans-serif, system-ui, sans-serif;
```

### Type Scale
```css
/* Headings */
h1: text-3xl font-bold tracking-tight (30px, 700 weight)
h2: text-2xl font-bold (24px, 700 weight)
h3: text-xl font-semibold (20px, 600 weight)
h4: text-lg font-semibold (18px, 600 weight)

/* Body Text */
body: text-sm (14px, 400 weight)
large: text-base (16px, 400 weight)
small: text-xs (12px, 400 weight)

/* Labels */
label: text-sm font-semibold (14px, 600 weight)

/* Code */
code: text-sm font-mono (14px, monospace)
```

### Text Colors
- **Primary Text**: `text-gray-900` (headings, important content)
- **Secondary Text**: `text-gray-600` (descriptions, metadata)
- **Muted Text**: `text-gray-500` (captions, placeholders)
- **Link Text**: `text-indigo-600 hover:text-indigo-800`

## 🧩 Component Library

All reusable UI components are defined in `src/Vira/Widgets.hs` with comprehensive documentation. Sometimes, it makes sense to define and move a component to `Widgets.hs` even if it is used once (for encapsulation & documentation purposes).

### Using Components

Always use these components instead of raw HTML to maintain design consistency:

```haskell
import Vira.Widgets qualified as W

-- Use components with proper imports
W.viraButton_ [type_ "submit"] "Save Changes"
W.viraCard_ [class_ "p-6"] $ do
  W.viraPageHeader_ "Settings" $ do
    p_ [class_ "text-gray-600"] "Configure your application"
```

### Component Documentation

Each component in `Widgets.hs` includes:
- **Purpose and design rationale**
- **Usage examples with code samples**  
- **Styling guidelines and color schemes**
- **Accessibility considerations**
- **When to use vs alternatives**

### Component Categories

See `src/Vira/Widgets.hs` for the complete component library:

- **Layout Components**: `viraSection_`, `viraCard_`, `viraPageHeader_`, `viraDivider_`
- **Interactive Components**: `viraButton_`, `viraIconButton_`, `viraInput_`, `viraLabel_`  
- **Display Components**: `viraStatusBadge_`, `viraCodeBlock_`, `viraCodeInline_`, `viraAlert_`, `viraFormGroup_`

### Creating New Components

When creating new components:
1. **Check existing widgets first** - Avoid duplication
2. **Follow naming convention** - `vira[ComponentName]_` pattern
3. **Include comprehensive documentation** - Follow existing examples
4. **Accept `[Attributes]` parameter** - For extensibility
5. **Use design system colors** - Follow the established palette
6. **Add to export list** - Make it available to other modules

## 🎛️ Layout Guidelines

### Spacing System
Use Tailwind's spacing scale consistently:
- `space-y-2` (8px) - Tight spacing within components
- `space-y-4` (16px) - Normal spacing between related elements
- `space-y-6` (24px) - Spacing between sections
- `space-y-8` (32px) - Large spacing between major sections

### Grid & Cards
```haskell
-- Repository grid
div_ [class_ "grid gap-6 md:grid-cols-2 lg:grid-cols-3"] $ do
  forM_ repos $ \repo ->
    W.viraCard_ [class_ "p-6"] $ do
      -- Card content

-- Two-column layout
div_ [class_ "grid gap-6 lg:grid-cols-2"] $ do
  W.viraCard_ [class_ "p-6"] leftContent
  W.viraCard_ [class_ "p-6"] rightContent
```

## 📱 Responsive Design

### Breakpoints
- Mobile: Default (< 768px)
- Tablet: `md:` (768px+)
- Desktop: `lg:` (1024px+)
- Large: `xl:` (1280px+)

## ♿ Accessibility Guidelines

All interactive elements must have visible focus indicators:
```css
focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2
```

- Ensure 4.5:1 contrast ratio for text
- Use proper heading hierarchy (h1 → h2 → h3)
- Include `alt` text for images and `title` for icon buttons

## 🚀 Implementation Guidelines

### Creating New Components
1. **Check existing widgets first** - Use components from `Widgets.hs`
2. **Follow naming convention** - `vira[ComponentName]_` for new widgets
3. **Accept `[Attributes]` parameter** - For extensibility
4. **Use design system colors** - Follow the established palette

### Code Style
```haskell
-- Good: Using design system
W.viraCard_ [class_ "p-6"] $ do
  W.viraPageHeader_ "Settings" $ do
    p_ [class_ "text-gray-600"] "Configure your application"

-- Bad: Inline styles without consistency
div_ [class_ "bg-white p-4 rounded shadow"] $ do
  h1_ [class_ "text-lg font-bold"] "Settings"
```

## 🔄 Status System

### Build Statuses
- **Running**: Blue (`bg-blue-100 text-blue-800`)
- **Pending**: Yellow (`bg-yellow-100 text-yellow-800`)
- **Success**: Green (`bg-green-100 text-green-800`)
- **Failed**: Red (`bg-red-100 text-red-800`)
- **Killed**: Darker red (`bg-red-200 text-red-900`)

### Connection Statuses
- **Connected**: Green alert with service details
- **Disconnected**: Blue info alert with setup instructions
- **Error**: Red alert with error message

## 📋 Common Patterns

### Empty States
```haskell
W.viraCard_ [class_ "p-12 text-center"] $ do
  div_ [class_ "text-gray-400 mb-4"] $ span_ [class_ "text-6xl"] "📦"
  h3_ [class_ "text-xl font-semibold text-gray-700 mb-2"] "No items yet"
  p_ [class_ "text-gray-500 mb-6"] "Description of what to do"
  -- Call to action button
```

---

**Remember**: This design system exists to maintain consistency and quality. When in doubt, refer to existing components in `Widgets.hs` or follow the patterns established in this document.
