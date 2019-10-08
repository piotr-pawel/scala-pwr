/*
1.  Zdefiniuj funkcję flatten : 'a list list -> 'a list, która dla argumentu będącego listą list
    tworzy listę, złożoną z elementów wszystkich podlist z zachowaniem ich kolejności,
    np. flatten [[5;6];[1;2;3]] zwraca [5; 6; 1; 2; 3], czyli spłaszcza listę o jeden poziom.
    Scala flatten: [A] (xss: List[List[A]]) List[A]
*/

def flatten [A](xss:List[List[A]]):List[A] = {

  if(xss.length == 0) Nil
  else xss.head ::: flatten(xss.tail)
}

flatten(List(List(5,6), List(1,2,3)))


/*
2.  Zdefiniuj funkcję count : 'a * 'a list -> int obliczającą ile razy dany element występuje
    w danej liście, np. count ('a', ['a'; 'l'; 'a']) zwraca 2.
    Scala count: [A ] (x: A, xs: List[A]) Int
*/

def count [A](x:A, xs:List[A]):Int = { 
  
  if(xs.length == 0) 0
  
  else 
    if(xs.head == x) 1 + count(x, xs.tail)
    else 0 + count(x, xs.tail)
}

count('a', Nil)
count('a', List('a','l','a'))
count('a', List('a','l','a','g','z','a','d','t'))

/*
3.  Zdefiniuj funkcję replicate: 'a * int -> 'a list powtarzającą dany obiekt określoną liczbę
    razy i zwracającą wynik w postaci listy, np. replicate ("la",3) zwraca ["la"; "la"; "la"].
    Scala replicate: [A] (x: A, n: Int) List[A]
*/

def replicate [A](x:A, n:Int):List[A] = {

  if(n == 0) Nil
  else List(x) ::: replicate(x, n-1) 
}

replicate("la", 3)
replicate('z', 6)

/*
4.  Zdefiniuj funkcję sqrList : int list -> int list podnoszącą do kwadratu wszystkie elementy
    danej listy liczb, np. sqrList [1;2;3;-4] zwraca [1; 4; 9; 16].
    Scala sqrList: (xs: List[Int]) List[Int] (metoda)
    lub sqrList: List[Int] => List[Int] (funkcja)
*/

//Metoda
def sqrList(xs: List[Int]):List[Int] = {
  
  if(xs.length == 0) Nil
  else List(xs.head * xs.head) ::: sqrList(xs.tail)
}

sqrList(List(1,2,3,-4))
sqrList(List(-1,-2,-3,4,-7,-11))

//Funkcja
((xs: List[Int]) => {
    if(xs.length == 0) Nil
    else (xs.head * xs.head :: Nil) ::: sqrList(xs.tail)
})(List(1,2,3,-4))


/*
5.  Zdefiniuj funkcję palindrome : 'a list -> bool sprawdzającą, czy dana lista jest
    palindromem, tj. równa się sobie samej przy odwróconej kolejności elementów,
    np. palindrome ['a'; 'l'; 'a'] zwraca true.
    Scala palindrome: [A] (xs: List[A]) Boolean
*/

def palindrome[A](xs:List[A]):Boolean = {
  
  if(xs.length <= 1) true
  
  else
  {
    val start = xs.head
    val list = xs.tail.reverse
    val end = list.head
    
    (start == end) && palindrome(list.tail)
  }
}

palindrome(List('a','l','a'))
palindrome(List('a','b','c','c','b','a'))
palindrome(List('a','b','c','d','b','a'))


/*
6.  Zdefiniuj swoją funkcję listLength : 'a list -> int, obliczającą długość dowolnej listy
    (oczywiście bez użycia standardowej funkcji List.length).
    Scala listLength: [A](xs: List[A])Int
*/

def listLength [A](xs:List[A]):Int = {
  if(xs == Nil) 0
  else 1 + listLength(xs.tail)
}

listLength(List('a','b','c','d','e','f','g'))





