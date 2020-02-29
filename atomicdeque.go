package tonixxx

import (
	"github.com/oleiade/lane"

	"sync"
)

// AtomicDeque provides a thread-safe Deque.
type AtomicDeque struct {
	// runningRecipesLock prevents race conditions.
	runningRecipesLock sync.Mutex

	// runningRecipes tracks running recipes
	runningRecipes *lane.Deque
}

// NewAtomicDeque constructs an AtomicDeque.
func NewAtomicDeque() *AtomicDeque {
	return &AtomicDeque{runningRecipes: lane.NewDeque()}
}

// Size queries the number of elements in the deque.
func (o *AtomicDeque) Size() int {
	defer o.runningRecipesLock.Unlock()
	o.runningRecipesLock.Lock()
	return o.runningRecipes.Size()
}

// Append inserts an element into the deque.
func (o *AtomicDeque) Append(element interface{}) {
	defer o.runningRecipesLock.Unlock()
	o.runningRecipesLock.Lock()
	o.runningRecipes.Append(element)
}

// Pop queries an end element from the deque and removes the element.
func (o *AtomicDeque) Pop() interface{} {
	defer o.runningRecipesLock.Unlock()
	o.runningRecipesLock.Lock()
	return o.runningRecipes.Pop()
}
