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

All reusable UI components are defined in `src/Vira/Widgets.hs`. Always use these components for consistency:

### Core Components

#### Layout Components
- `viraSection_` - Page section wrapper with consistent spacing
- `viraCard_` - Card container with elegant shadows and rounded corners
- `viraPageHeader_` - Standardized page headers with title and subtitle
- `viraDivider_` - Visual content separator

#### Interactive Components
- `viraButton_` - Primary action buttons with hover states
- `viraIconButton_` - Icon-only buttons for secondary actions
- `viraInput_` - Form input fields with proper focus states
- `viraLabel_` - Form labels with consistent typography

#### Display Components
- `viraStatusBadge_` - Status indicators with semantic colors
- `viraCodeBlock_` - Code display with proper formatting
- `viraAlert_` - Alert messages (success, error, warning, info)
- `viraFormGroup_` - Form field grouping for consistent layouts

### Component Usage Guidelines

#### Buttons
```haskell
-- Primary action (most important action on page)
W.viraButton_ [class_ "bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"] "Save Changes"

-- Success action
W.viraButton_ [class_ "bg-green-600 hover:bg-green-700 focus:ring-green-500"] "✅ Build"

-- Destructive action
W.viraButton_ [class_ "bg-red-600 hover:bg-red-700 focus:ring-red-500"] "🗑️ Delete"

-- Secondary action
W.viraIconButton_ [] "⚙️"
```

#### Status Badges
```haskell
-- Success
W.viraStatusBadge_ "Success" "bg-green-100 text-green-800 border-green-200"

-- Error
W.viraStatusBadge_ "Failed" "bg-red-100 text-red-800 border-red-200"

-- Warning/Pending
W.viraStatusBadge_ "Pending" "bg-yellow-100 text-yellow-800 border-yellow-200"

-- Info/Running
W.viraStatusBadge_ "Running" "bg-blue-100 text-blue-800 border-blue-200"
```

#### Alerts
```haskell
-- Success message
W.viraAlert_ "success" "bg-green-50 border-green-200" $ do
  p_ [class_ "text-green-800"] "Repository successfully added!"

-- Error message
W.viraAlert_ "error" "bg-red-50 border-red-200" $ do
  p_ [class_ "text-red-800"] "Failed to connect to repository"
```

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

### Page Structure
```haskell
pageContent = do
  W.viraSection_ [] $ do
    -- Page header
    W.viraPageHeader_ "Page Title" $ do
      p_ [class_ "text-gray-600"] "Page description"
    
    -- Main content cards
    W.viraCard_ [class_ "p-6 mb-6"] $ do
      -- Primary content
    
    W.viraCard_ [class_ "p-6"] $ do
      -- Secondary content
```

## 🎭 Visual Effects

### Shadows
```css
/* Component shadows */
.shadow-elegant: box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);

/* Card hover states */
hover:shadow-lg
hover:shadow-xl
```

### Gradients
```css
/* Background gradient */
bg-gradient-to-br from-slate-50 to-blue-50

/* Header gradients */
bg-gradient-to-r from-indigo-600 via-purple-600 to-blue-600
bg-gradient-to-r from-indigo-50 to-blue-50
```

### Transitions
```css
/* Smooth transitions for all interactive elements */
.transition-smooth: transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
```

### Glass Morphism
```css
/* For overlay elements */
backdrop-blur-sm
bg-white/80
border border-white/20
```

## 📱 Responsive Design

### Breakpoints
- Mobile: Default (< 768px)
- Tablet: `md:` (768px+)
- Desktop: `lg:` (1024px+)
- Large: `xl:` (1280px+)

### Responsive Patterns
```haskell
-- Responsive grid
div_ [class_ "grid gap-4 md:grid-cols-2 lg:grid-cols-3"] $ do

-- Responsive padding
div_ [class_ "p-4 lg:p-8"] $ do

-- Responsive text
h1_ [class_ "text-2xl lg:text-3xl"] $ do
```

## ♿ Accessibility Guidelines

### Focus States
All interactive elements must have visible focus indicators:
```css
focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2
```

### Color Contrast
- Ensure 4.5:1 contrast ratio for normal text
- Ensure 3:1 contrast ratio for large text
- Don't rely solely on color to convey information

### Semantic HTML
- Use proper heading hierarchy (h1 → h2 → h3)
- Use semantic elements (`nav`, `main`, `section`, `article`)
- Include `alt` text for images
- Use `role` attributes when needed

## 🚀 Implementation Guidelines

### When Creating New Components
1. **Check existing widgets first** - Use components from `Widgets.hs`
2. **Follow naming convention** - `vira[ComponentName]_` for new widgets
3. **Include proper attributes** - Accept `[Attributes]` parameter
4. **Use design tokens** - Follow the color palette and spacing system
5. **Add hover/focus states** - Include interactive feedback
6. **Test responsiveness** - Ensure it works on all screen sizes

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

### Class Naming Patterns
- **Layout**: `p-6`, `m-4`, `space-y-6`, `grid`, `flex`
- **Colors**: `bg-white`, `text-gray-900`, `border-gray-200`
- **Interactions**: `hover:bg-gray-50`, `focus:ring-2`, `transition-smooth`
- **Responsive**: `md:grid-cols-2`, `lg:p-8`, `xl:max-w-6xl`

## 🔄 Status System

### Build Statuses
- **Running**: Blue (`bg-blue-100 text-blue-800`) with "🚧" or spinner
- **Pending**: Yellow (`bg-yellow-100 text-yellow-800`) with "⏳"
- **Success**: Green (`bg-green-100 text-green-800`) with "✅"
- **Failed**: Red (`bg-red-100 text-red-800`) with "❌"
- **Killed**: Gray (`bg-gray-100 text-gray-800`) with "💀"

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

### Loading States
```haskell
-- Button loading state
W.viraButton_ [disabled_ "", class_ "opacity-50 cursor-not-allowed"] $ do
  "⏳ Loading..."
```

### Form Validation
```haskell
-- Error state
W.viraAlert_ "error" "bg-red-50 border-red-200" $ do
  p_ [class_ "text-red-800"] "Please fix the following errors:"

-- Success state
W.viraAlert_ "success" "bg-green-50 border-green-200" $ do
  p_ [class_ "text-green-800"] "Settings saved successfully!"
```

---

**Remember**: This design system exists to maintain consistency and quality. When in doubt, refer to existing components in `Widgets.hs` or follow the patterns established in this document. Always prioritize user experience and accessibility in your implementations.
