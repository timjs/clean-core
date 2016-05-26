implementation module Core

import Core

a :: {Int}
a = {1,2,3}

b :: {!Int}
b = {4,5,6}

c :: {#Int}
c = {7,8,9}

Start = (a, a ++ a, b, b ++ b, c, c ++ c)
