# haskeme

A Scheme interpreter written in Haskell

```
  _    _           _                       
 | |  | |         | |                      
 | |__| | __ _ ___| | _____ _ __ ___   ___ 
 |  __  |/ _` / __| |/ / _ \ '_ ` _ \ / _ \
 | |  | | (_| \__ \   <  __/ | | | | |  __/
 |_|  |_|\__,_|___/_|\_\___|_| |_| |_|\___|
```

### Running locally

Make sure you have stack tool installed or you could go here to install: [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

```
$ git clone https://github.com/abiduzz420/haskeme.git && cd haskeme
$ stack build --test
$ stack install
$ haskeme
```

### Installing via Docker

```
$ docker pull lambdastyle/haskeme
$ docker run -it lambdastyle/haskeme
```

### Demo

```
$ haskeme
λ> (* 1 2 3 4)
24
λ> (define ten 10)
10
λ> (+ ten 10)
20
λ> (define (double x) (* x 2))
(lambda ("x") ...)
λ> (double ten)
20 
λ> (equal? 23 23)
#t
λ> (&& #t #f)
#f
λ> (|| #t #f)
#t
λ> (string=? "foo" "foo")    
#t
λ> (define primes '(3 5 7 11))
(3 5 7 11)
λ> (define morePrimes (cons 2 primes))
(2 3 5 7 11)
λ> (car morePrimes)
2
λ> (cdr morePrimes)
(3 5 7 11)
```
### References

[1] [Write Yourself a Scheme in 48 Hours by Jonathan Tang](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) (SPOILER ALERT: It took me more than just 48 hours)

[2] [Lisper project by Jaseem Abid](https://github.com/jaseemabid/lisper)

[3] [Little Schemer by Daniel P. Friedman and Matthias Felleisen](https://7chan.org/pr/src/The_Little_Schemer_4th_2.pdf)

### Changelog

- IO functionality and standard scheme library implementation (currently buggy)
- support for scheme functions
- built repl and defining variables
- added error handling support
- added primitive functionalities