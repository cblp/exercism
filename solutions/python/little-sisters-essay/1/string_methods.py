"""Functions to help edit essay homework using string manipulation."""


from functools import partialmethod


capitalize_title = str.title


def check_sentence_ending(sentence):
    """Check the ending of the sentence to verify that a period is present.

    :param sentence: str - a sentence to check.
    :return: bool - return True if punctuated correctly with period, False otherwise.
    """

    return sentence.endswith('.')


clean_up_spacing = str.strip


replace_word_choice = str.replace
