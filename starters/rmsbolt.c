#include <stdio.h>

// C rmsbolt starter file

// Local Variables:
// rmsbolt-command: "gcc -O0"
// rmsbolt-disassemble: nil
// End:

int isRMS(int a) {
	 switch (a) {
	 case 'R':
		  return 1;
	 case 'M':
		  return 2;
	 case 'S':
		  return 3;
	 default:
		  return 0;
	 }
}

int main() {
		char a = 1 + 1;
		if (isRMS(a))
			 printf("%c\n", a);
}
