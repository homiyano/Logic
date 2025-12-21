% ------------ colors -----------

color(red).
color(green).
color(black).
color(blue).

warm(red).
cool(blue).

favorite(C) :- warm(C).
favorite(C) :- cool(C).

% ------------ likes -----------

likes(homi, apple).
likes(homi, banana).
likes(ghazi, banana).

likes_fruit(Person, Fruit) :-
    likes(Person, Fruit).

happy(Person) :-
    likes(Person, banana),
    likes(Person, apple).

% ------------ numbers -----------

num(1).
num(2).
num(3).

even(2).

good(X) :-
    num(X),
    even(X).