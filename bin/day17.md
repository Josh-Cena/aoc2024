# Day 17

This problem reminds me of 2021 Day 24, where we also have to stare at assembly and figure out some pattern, so here it is.

```plain
bst %a # B = A & 0b111
bxl $X # B = B ^ X
cdv %b # C = A >> B
bxl $Y # B = B ^ Y
bxc $4 # B = B ^ C
out %b # output B & 0b111
adv $3 # A = A >> 3
jnz $0 # If A != 0, loop
```

I'm redacting `X` and `Y` because they are personalized. Anyway, this code is just the following in C (because OCaml is terrible at describing bitwise operations):

```c
do {
    int b = a & 0b111;
    int c = a >> (b ^ x);
    b = b ^ x ^ y ^ c;
    printf("%d,", b & 0b111);
    a = a >> 3;
} while (a != 0);
```

Even simpler:

```c
do {
    int ao = a & 0b111; // Last 3 bits of a
    printf("%d,", ((a >> (ao ^ x)) ^ a ^ x ^ y) & 0b111);
    a = a >> 3;
} while (a != 0);
```

Let's assume `x = 1`, `y = 2` (they aren't for my input). Denote the bits of `a` from lowest to highest (at this particular iteration) as `[a0]`, `[a1]`, etc. The output is then:

| `ao`  | `(a >> (ao ^ x)) ^ ao ^ x ^ y` |
| ----- | ------------------------------ |
| `000` | `[a3]11`                       |
| `001` | `011`                          |
| `010` | `[a5][a4][~a3]`                |
| `011` | `[a4][a3]0`                    |
| `100` | `[~a7][~a6][~a5]`              |
| `101` | `[~a6][~a5][a4]`               |
| `110` | `[~a9][a8][~a7]`               |
| `111` | `[~a8][a7][a6]`                |

The expected output is:

```plain
2,4,1,X,7,5,1,Y,4,4,5,5,0,3,3,0
```

Since we want to find the smallest `a`, we should start with the most significant bits and work our way down. The last output is `0`, and we can pick all rows in the table that don't contain any 1 bits. Since higher bits than the MSB are necessarily `0`, the only solution is `ao = 011`. That's the highest octet of `a`. Now we can pick the next octet equipped with this knowledge, and so on. Sometimes we may have multiple choices, and we use a DFS to figure out the first valid solution.
