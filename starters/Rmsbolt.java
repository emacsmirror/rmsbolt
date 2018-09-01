// Java rmsbolt starter file

// Local Variables:
// rmsbolt-command: "javac"
// rmsbolt-filter-directives: t
// End:

public class Rmsbolt {
	public static int isRMS(char in) {
		switch (in) {
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

	public static void main(String[] args) {
		char a = 1 + 1;
		if (isRMS(a) == 0)
			System.out.println(a);
	}
}
