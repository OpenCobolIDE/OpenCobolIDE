"""
A resetable memoize decorator, taken from: http://stackoverflow.com/questions/4431703/python-resettable-instance-method-memoization-decorator
"""
import functools


class memoize(object):
    def __init__(self, func):
        self.func = func
        self.memoized = {}
        self.method_cache = {}

    def __call__(self, *args):
        return self.cache_get(self.memoized, args,
                              lambda: self.func(*args))

    def __get__(self, obj, objtype):
        return self.cache_get(self.method_cache, obj, lambda: self.__class__(functools.partial(self.func, obj)))

    def cache_get(self, cache, key, func):
        try:
            return cache[key]
        except KeyError:
            cache[key] = func()
            return cache[key]


class memoized(object):
    """Decorator that caches a function's return value each time it is called.
    If called later with the same arguments, the cached value is returned, and
    not re-evaluated.
    """
    def __init__(self, func):
        self.func = func
        self.cache = {}

    def __call__(self, *args):
        try:
            return self.cache[args]
        except KeyError:
            value = self.func(*args)
            self.cache[args] = value
            return value
        except TypeError:
            # uncachable -- for instance, passing a list as an argument.
            # Better to not cache than to blow up entirely.
            return self.func(*args)

    def __repr__(self):
        """Return the function's docstring."""
        return self.func.__doc__

    def __get__(self, obj, objtype):
        """Support instance methods."""
        fn = functools.partial(self.__call__, obj)
        fn.reset = self.reset
        return fn

    def reset(self):
        self.cache = {}


if __name__ == '__main__':
    import time

    class my_class:
        @classmethod
        @memoized
        def my_func(cls, val):
            print("in my_func")
            time.sleep(2)
            return val


    c = my_class()

    print("should take time")
    print(c.my_func(55))
    print()

    print("should be instant")
    print(c.my_func(55))
    print()

    c.my_func.reset()

    print("should take time")
    print(c.my_func(55))