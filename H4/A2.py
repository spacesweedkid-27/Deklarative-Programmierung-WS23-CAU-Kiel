# Works
def filtr(condition, lst):
    temp = []
    for elem in lst:
        if (condition(elem)):
            temp.append(elem)
    return temp

# Works
def foldl(operation, start, lst):
    temp = start
    for elem in lst:
        temp = operation(temp, elem)
    return temp

# Works
def zipWith(operation, lst_a, lst_b):
    temp = []
    # funny that this function is also called zip
    # I just wanted to use this to spare some bits and bytes, but
    # you can also just iterate over an integer that increases until
    # the shorter list's length is equal to the current index.
    for (a, b) in zip(lst_a, lst_b):
        temp.append(operation(a,b))
    return temp
    

"""TESTING"""
#a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
#b = [4, 7, 10]
#print("All even numbers in the range 1 to 10:")
#print(filtr(lambda x: x % 2 == 0, a))
#print("The product of the numbers 1 to 10:")
#print(foldl(lambda z, x: z * x, 1, a))
#print("Zipping two lists by multiplying their elements:")
#print(zipWith(lambda x, y: x * y, a, b))