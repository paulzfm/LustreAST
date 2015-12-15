# -*- coding: utf-8 -*-
import re
class Refiner(object):

    def __init__(self, mll_path):
        self.mll_path = mll_path
        self.extract()
        self.check_if_one_keyword_contains_another()
        assert self.check_repeat()

    def extract(self):
        self.keyword_index = []
        with open(self.mll_path) as f:
            for each in f.read().splitlines():
                if '#' in each:
                    target_str = each.split()[1][2:-1]
                    self.keyword_index.append(target_str)

    def check_if_one_keyword_contains_another(self):
        self.ignore = []
        for eachx in self.keyword_index:
            for eachy in self.keyword_index:
                if eachx == eachy:
                    continue
                if eachx in eachy:
                    self.ignore.append(eachy)

    def check_repeat(self):
        for i in xrange(len(self.keyword_index)):
            for j in xrange(i+1, len(self.keyword_index)):
                if self.keyword_index[i] == self.keyword_index[j]:
                    print self.keyword_index[i]
                    return False
        return True

    def handle(self, input_file):
        source = open(input_file).read()

        for each in self.keyword_index:
            pattern = r'[^\w_]' + each + r'[^\w_]'
            matches=re.finditer(pattern,source)
            k = 1
            for each in matches:
                index = each.start() + k
                k += 1
                source = self.insert_dash(source, index)
            #source = source.replace(each, '#'+each)
        print source
        return source

    @staticmethod
    def insert_dash(string, index):
        return string[:index] + '#' + string[index:]


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-m', '--MLL_PATH', default='./lexer.mll')
    parser.add_argument('-i', '--INPUT_PATH')
    args = parser.parse_args()
    r = Refiner(args.MLL_PATH)
    r.handle(args.INPUT_PATH)






