# TODO

*   Broadcasting
*   Mapping
*   `tfold`
*   what's in the monad? Ex. don't want to have to use `<-` every time use function, like
	```
	c <- f1 a b
	d <- f2 a c
	```
	want to do
	```
	d <- save $ f2 a (f1 a b)
	```
	This "compilation" should be done on the level of tensors---the graph is only to keep track of references.
