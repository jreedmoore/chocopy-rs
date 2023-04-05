# A broken program
def is_even(x:int) -> bool:
    if x % 2 == 1:
        return 0      # FIXME
    else:
        return True

print(is_even("3"))   # FIXME