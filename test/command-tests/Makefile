all:
	../../ez-subst -t t0.toml && diff t0.output t0.expected
	../../ez-subst -t t1.toml && diff t1.output t1.expected
	../../ez-subst input.txt -c c0.toml -o t2.output && diff t2.output t2.expected
	../../ez-subst input.txt  -c c0.toml -c t0.toml -c t1.toml  -o t3.output && diff t3.output t3.expected
	../../ez-subst input.txt  -c c0.toml -c t0.toml -c t1.toml --var x=Hi  -o t4.output && diff t4.output t4.expected
	../../ez-subst -t t5.toml && diff t5.output t5.expected
	../../ez-subst -t t6.toml && diff t6.output t6.expected

clean:
	rm -f *.output *~

