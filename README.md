UNFINISHED!

---

Have you finished The Reasoned Schemer?

#f

---

That's okay.

#t

---

What's this?

It's a program that uses logic to generate alkanes.

---

What's the value of `(run1 (q) (alkaneo '((() . ())) '(1) q))`

'((1))

---

What's the value of `(run1 (q) (alkaneo '((() . ()) (() . (((() . ())))) (() . ())) q '(0 0 1)))`

`'((1 1))`

---

What does these parameters mean?

The first parameter is the alkane,
which is represented by a list of pair of substituent.
The second one is the length of the main carbon chain.
The third one is the total number of carbon.

---

What's the value of `(run1 (q) (alkaneo q '(0 1) '(0 1)))`

`'(((()) (())))`

