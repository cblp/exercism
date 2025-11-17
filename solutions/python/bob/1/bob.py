def response(hey_bob):
    hey_bob = hey_bob.strip()
    letters = [c for c in hey_bob if c.isalpha()]
    is_yell = letters and all(c.isupper() for c in letters)
    is_question = hey_bob.endswith("?")
    is_silence = hey_bob == ""
    return (
        "Calm down, I know what I'm doing!" if is_yell and is_question else
        "Sure."                             if is_question             else
        "Whoa, chill out!"                  if is_yell                 else
        "Fine. Be that way!"                if is_silence              else
        "Whatever."
    )
