package main

import (
	"fmt"
)

// Go rmsbolt starter file

// Local Variables:
// rmsbolt-command: "go"
// rmsbolt-disassemble: t
// End:

func isRMS(a int) int {
	switch a {
	case 'R':
		return 1
	case 'M':
		return 2
	case 'S':
		return 3
	default:
		return 0
	}
}

func main() {
	a := 1 + 1
	if isRMS(a) != 0 {
		fmt.Printf("%c\n", a)
	}
}
