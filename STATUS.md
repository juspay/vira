
## Goal

I want to revamp our status update system to be more general.  https://github.com/juspay/vira/issues/109

Presently, there is a build status indicator on top-right of the page that is updated in real time (see `Vira.Stream.Status`). But the rest of the page, like build status ("Running", "Killed", etc.) do not automatically update.

Instead of auto-updating each and every part of the page, I figured it is best to create a general status system based on acid-state `update` events. The `SafeCopy` instances for all the acid-state actions (e.g.: `JobUpdateStatusA`) allow us to serialize and deserialize them through a queue (perhaps `CircularBuffer`?) in the central `AppState`. Then, any part of the application (like `Vira.Stream.Status`) can "subscribe" to them for getting real-time status.

From HTMX standpoint, it may be beneficial to have the top-level `<body>` to create a single SSE connection subscribing to all events, and then sub-elements below can look for specific event types.

## Approach

### Implementation Steps

Each step can be developed and verified independently.

#### 1. Event System & Queue
Add `CircularBuffer` to `AppState` to queue existing acid-state actions (e.g. `JobUpdateStatusA`) as events, using their existing `SafeCopy` instances. This could potentially happen in `Vira.App.AcidState::update`.

#### 2. Single SSE Stream
Replace per-component SSE connections with one body-level stream broadcasting all events with event type filtering.

#### 3. HTMX Integration
Update HTMX to use single SSE connection with event filtering for sub-elements to subscribe to specific event types.

#### 4. Migration
Incrementally migrate existing status displays to the new system while maintaining backward compatibility.
