#1

import math

def greet(classname):
    print (classname)
greet('Two')
greet('Three')
greet('Four')

#2

n1 = int (input("Enter the first number: "))
n2 = int (input("Enter the second number: "))
n3 = int (input("Enter the third number: "))
average = (n1 + n2 + n3) / 3
print ("the average is: " , average )


#3
def numdefine (num):
    if num >= 0:
        if num == 0:
            print("Negative number")
        else:
            if num % 2 ==0:
               print("Zero but Odd")
               print("Kuchvi")
            elif (num % 3)==0:
               print ("Zero but Even")
            else:
               print("Nothing")
        print ("Not Positive Number")
    else:
        if num < 0:
            print("Not Negative number")
        else:
            if num % 2 ==1:
               print("One but Even")
               print("Kuchvi")
            elif (num % 3)==1:
               print ("One but Even")
            else:
                x=1
                print("Kuchvi")
        print("Positive number")

a=3
b=7
c=12
d=9
b=a+b
c=c+d+a-b
a=b*a
d=b-a
numdefine (b)

#4

num = 10
for i in range(1, 11):
   print(num,'x',i,'=',num*i)

#5

def f(x):
   for i in range(1, x + 1):
       if x % i == 0:
           print(i)

f(20)

#6

def pypart(n):
    for i in range(0, n):
        for j in range(0, i+1):
            print("* ",end="")
        print("\r")

n = 5
pypart(n)

#7

side=5
for i in range(side):
    for i in range(side):
        print('*', end = '  ')
    print()

#8

a=16.0
b=8.0
b+=8.0
c=2.0
c*=1
a/=2.0
d=5.0
d-=1
print(a-b)
print(a+b)
print(a/b)
print(a*b)
print(a%b)
print(b**c)
print(b//a)
print(a/b==c)
print(a!=c)
print(a>c)
print(b>=a)
print (d<a)
print (c<=d)

#9
for x in range(5):
     print (x)
#10
for val in "string":
    if val == "i":
        break
    print(val)

print("The end")

for val in "string":
    if val == "i":
        continue
    print(val)

print("The end")

#11

a=-4.67
print (math.ceil(a))
print (math.floor(a))
print (abs(a))
print (round(a))

#12

s="abc"
t="$"
u="$"
v="$"
w="$"
z="$"
x=2
for i in s:
    t=" "+i
    u=i*2
    if 'c' in z:
        v='#'
    if i not in w:
        w=s[x]+"$"
    z=z+t+u+v+w
print (z)

#13

my_list = ['h',10, "mouse", [8, 4, 6], ['a'], 23.89, ["234", "hello"]]
print(my_list[4])
print(my_list[2][1])
print(my_list[4][0])
print(my_list[-1])
print(my_list[-5])
print(my_list[2:5])
print(my_list[:-5])
print(my_list[5:])
print(my_list[:])
my_list[1:4] = [3, 5, 7]
del my_list[1:5]
my_list.remove('h')
print(my_list)
print(my_list)
print(my_list + [9, 7, 5])

#14
a=4
def f(a):
    return h(a*2)
def j(a):
    a=100
    return f(a/2)
def g(a):
    a=10
    return f(a/2)
def i(a=100):
    return a//2
def h(a):
    return a**2
print (g(a))


#15

lst=[1,2,3,4,5,1,2,6,7,3,9]
lst.reverse()
print (lst)
lst.sort()
print (lst)
