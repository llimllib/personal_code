#!/usr/bin/env python

def xnutple(letter_list, max_len=9999, min_len=1):
    #NOTE: we assume that all elements in letter_list are unique
    if letter_list == [] or min_len <= 0 or max_len <= 0 or max_len < min_len:
        raise ValueError, "Please pass a list of strings and two lengths > 0"

    list_len = len(letter_list)
    last_letter = letter_list[-1]
    list_ptr = 0
    cur_char_list = [letter_list[0]] * min_len

    while len(cur_char_list) <= max_len:
        yield ''.join(cur_char_list)

        #go to the next letter
        list_ptr += 1
        
        #if the list_ptr can be incremented, set the last letter of cur_char_list to the next letter
        if list_ptr < list_len:
            cur_char_list[-1] = letter_list[list_ptr]
        
        #otherwise, reset the list pointer, and go backwards until we find a 
        #non-last letter
        else:
            list_ptr = 0

            #count backwards from the end of the list and incremement the first
            #non-last element, setting all others to letter_list[0]
            #in other words, if our letters are [a,b,c] and cur_char_list == 
            #['a', 'b', 'c']", find 'b' and
            #make it a 'c'
            i = len(cur_char_list) - 1
            while i >= 0:
                if cur_char_list[i] == last_letter:
                    cur_char_list[i] = letter_list[0]
                else:
                    #increment the letter
                    idx = letter_list.index(cur_char_list[i])
                    cur_char_list[i] = letter_list[idx+1]
                    break
                i -= 1

            if i < 0:
                #we're at "ccc", so change to "aaaa" and start again
                new_len = len(cur_char_list) + 1
                cur_char_list = [letter_list[0]] * new_len

def test():
    import traceback
    exceptions = [
        #(input, expected output)
        (([],), ValueError),
        (([], -1), ValueError),
        (([], -1, -1), ValueError),
        ((['a'], -1, 1), ValueError),
        ((['a'], 0, 1), ValueError),
        ((['a'], 1, 0), ValueError),
        ((['a'], 1, -1), ValueError),
    ]
    tests = [
        #(input, output)
        ((['a'], 1), set(['a'])),
        ((['a', 'b'], 1), set(['a', 'b'])),
        ((['a', 'b'], 2), set(['a', 'b', 'aa', 'ab', 'ba', 'bb'])),
        ((['a', 'b'], 2, 2), set(['aa', 'ab', 'ba', 'bb'])),
        ((['a', 'b', 'c'], 3, 2), set(['aa', 'ab', 'ac', 'ba', 'bb', 'bc', 'ca',
            'cb', 'cc', 'aaa', 'aab', 'aac', 'aba', 'abb', 'abc', 'aca', 'acb',
            'acc', 'baa', 'bab', 'bac', 'bba', 'bbb', 'bbc', 'bca', 'bcb', 
            'bcc', 'caa', 'cab', 'cac', 'cba', 'cbb', 'cbc', 'cca', 'ccb', 
            'ccc']))
    ]
    functions_to_test = [xntuple]

    tests_run = 0
    passed = 0
    failed = 0

    for test_func in functions_to_test:
        for input, exceptionType in exceptions:
            tests_run += 1
            try:
                list(test_func(*input))
                failed += 1
                print "No exception raised by %s, expected %s (in function %s)" \
                    % (input, output, test_func.__name__)
            except exceptionType:
                passed += 1
            except:
                failed += 1
                print """Incorrect Exception type in function %s. On input %s, expected %s""" % \
                    (test_func.__name__, input, exceptionType)
                print traceback.print_exc()

        for input, expected_output in tests:
            tests_run += 1
            try:
                actual_output = tuple(test_func(*input))
                output_set = set(actual_output)
                #make sure we didn't generate any dupes, and that we generated
                #the proper values
                if len(actual_output) == len(output_set) \
                and output_set == expected_output:
                    passed += 1
                else:
                    failed += 1
                    print "On input %s, Expected %s, got %s (function %s)" \
                        % (input, expected_output, actual_output,
                        test_func.__name__)
            except Exception, e:
                failed += 1
                traceback.print_exc()
                print "On input %s in function %s, got an exception: %s" % \
                    (input, test_func.__name__, e.message)

    print "%s tests run, %s passed, %s failed" % (tests_run, passed, failed)

if __name__ == "__main__":
    test()
