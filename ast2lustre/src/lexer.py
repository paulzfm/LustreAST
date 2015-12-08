# -*- coding: utf-8 -*-
MLL_PATH = '/Users/megvii/Desktop/LustreAST/ast2lustre/src/lexer.mll'
INPUT_PATH = '/Users/megvii/Desktop/LustreAST/ast2lustre/src/binary_add.ast'


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
        with open(input_file, 'w') as f:
            for each in self.keyword_index:
                if each in self.ignore:
                    continue
                source = source.replace(each, '#'+each)
            f.write(source)


if __name__ == '__main__':
    r = Refiner(MLL_PATH)
    r.handle(INPUT_PATH)
