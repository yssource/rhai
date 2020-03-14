// This is a script to calculate prime numbers.

const MAX_NUMBER_TO_CHECK = 10000;    // 1229 primes

let prime_mask = [];
prime_mask.pad(MAX_NUMBER_TO_CHECK, true);

prime_mask[0] = false;
prime_mask[1] = false;

let total_primes_found = 0;

for p in range(2, MAX_NUMBER_TO_CHECK) {
    if prime_mask[p] {
        print(p);

        total_primes_found += 1;
        let i = 2 * p;

        while i < MAX_NUMBER_TO_CHECK {
            prime_mask[i] = false;
            i += p;
        }
    }
}

print("Total " + total_primes_found + " primes.");

