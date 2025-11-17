"""Functions to manage a users shopping cart items."""

from typing import Iterable, cast


Cart = dict[str, int]


def add_item(cart: Cart, items: Iterable[str]) -> Cart:
    """Add items to shopping cart.

    :param cart: dict - the current shopping cart.
    :param items: iterable - items to add to the cart.
    :return: dict - the updated user cart dictionary.
    """

    for item in items:
        cart[item] = cart.get(item, 0) + 1
    return cart


def read_notes(notes: Iterable[str]) -> Cart:
    """Create user cart from an iterable notes entry.

    :param notes: iterable of items to add to cart.
    :return: dict - a user shopping cart dictionary.
    """

    return add_item({}, notes)


Ideas = dict[str, dict[str, int]]


def update_recipes(
    ideas: Ideas, recipe_updates: Iterable[tuple[str, dict[str, int]]]
) -> Ideas:
    """Update the recipe ideas dictionary.

    :param ideas: dict - The "recipe ideas" dict.
    :param recipe_updates: iterable -  with updates for the ideas section.
    :return: dict - updated "recipe ideas" dict.
    """

    ideas.update(recipe_updates)
    return ideas


def sort_entries(cart: Cart) -> Cart:
    """Sort a users shopping cart in alphabetically order.

    :param cart: dict - a users shopping cart dictionary.
    :return: dict - users shopping cart sorted in alphabetical order.
    """

    return dict(sorted(cart.items()))


Inventory = dict[str, list[object]]

InventoryOut = dict[str, list[object]]


def send_to_store(cart: Cart, aisle_mapping: dict[str, list[object]]) -> Inventory:
    """Combine users order to aisle and refrigeration information.

    :param cart: dict - users shopping cart dictionary.
    :param aisle_mapping: dict - aisle and refrigeration information dictionary.
    :return: dict - fulfillment dictionary ready to send to store.
    """

    return dict(
        sorted(
            (
                (item, [count, aisle, refrigeration_needed])
                for item, count in cart.items()
                for aisle, refrigeration_needed in [aisle_mapping[item]]
            ),
            reverse=True,
        )
    )


def update_store_inventory(
    fulfillment_cart: Inventory, store_inventory: Inventory
) -> InventoryOut:
    """Update store inventory levels with user order.

    :param fulfillment cart: dict - fulfillment cart to send to store.
    :param store_inventory: dict - store available inventory
    :return: dict - store_inventory updated.
    """

    return {
        item: [
            (
                (
                    cast(int, count)
                    - (
                        cast(int, fulfillment_cart[item][0])
                        if item in fulfillment_cart
                        else 0
                    )
                )
                or "Out of Stock"
            ),
            aisle,
            refrigeration_needed,
        ]
        for item, (count, aisle, refrigeration_needed) in store_inventory.items()
    }
