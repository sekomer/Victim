{-
	Pytagorean triples below 100
-}

for (var i := 1; i < 100; i := i + 1)
	for (var j := 1; j < 100; j := j + 1)
		for (var k := 1; k < 100; k := k + 1)
			if (i ** 2 + j ** 2 == k ** 2)
				print(i, j, k);