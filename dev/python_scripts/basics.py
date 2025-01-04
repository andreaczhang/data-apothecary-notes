# some notes 

# with input
cart = []

def add_to_cart(cart):
  product = input()
  cart.append(product)

add_to_cart(cart)
print(cart)


# function 2 ----

#empty list
new_menu = []

#dishes for the new menu
dish1 = "Pasta"
dish2 = "Pizza"
dish3 = "Salad"

#function definition
def add_to_menu(menu, dish):
    menu.append(dish)

#adding dish 1
add_to_menu(new_menu, dish1)
print(new_menu)

#adding dish 2
add_to_menu(new_menu, dish2)
print(new_menu)

#adding dish 3
add_to_menu(new_menu, dish3)
print(new_menu)


ages = [25, 33, 19]
sorted(ages)


values = ['1', '2', '3']
print(sum(values))





