import java.util.function.BiFunction;
// package benchmarks.Java;

public class Sum {
    private static int sumMono(int n, int curr, int change) {
        while (n-- > 0) {
            curr += change;
        }

        return curr;
    }

    private static <T> T sumPoly(BiFunction<T, T, T> add, int n, T curr, T change) {
        while (n-- > 0) {
            curr = add.apply(curr, change);
        }

        return curr;
    }

    public static void main(String[] args) {
        if (args[2].equals("mono")) {
            System.out.println("Running sumMono...");
            System.out.println("" + sumMono(Integer.parseInt(args[1]), 12, 2));
        } else {
            System.out.println("Running sumPoly...");
            System.out.println("" + 
                sumPoly((x, y) -> x + y, Integer.parseInt(args[1]), 12, 2));
        }
    }
}
