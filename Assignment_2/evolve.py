#!/usr/bin/env python3

import sys
import numpy
import random

options = {'filename': '',
           'population_size': 100,
           'elite_percent': 20,
           'mutation_rate': 5,
           'num_generations': 100,
           'max_unchanged_generations': 100}

num_cities = 0
cities_data = numpy.array([])
start_city = 0
population = []


def get_options():
    cl_options = sys.argv[1:]
    options['filename'] = cl_options[0]
    for entry in cl_options[1:]:
        option, value = entry.strip('-').split('=')
        if option in options:
            options[option] = int(value)


class City:
    def __init__(self, number, x, y):
        self.number = number
        self.x = x
        self.y = y
        self.coords = (x, y)

    def __repr__(self):
        return "City({n}, {x}, {y})".format(n=self.number,
                                            x=self.x,
                                            y=self.y)

    def __str__(self):
        return "{n}".format(n=self.number)


class Segment:
    def __init__(self, a: City, b: City):
        self.a = a
        self.b = b
        self.dist = self.euclidean_dist()

    def __repr__(self):
        return "Segment({}, {})".format(self.a,
                                        self.b)

    def __str__(self):
        return "{}->{}, dist: {}".format(self.a.number,
                                         self.b.number,
                                         self.dist)

    def euclidean_dist(self):
        return numpy.linalg.norm(self.a.coords - self.b.coords)


class Circuit:
    def __init__(self, city_arr):
        self.segments = numpy.array(
            [Segment(a, b)
             for a, b in
             list(
                 zip(
                     city_arr[:-1],
                     city_arr[1:]))
             ])
        self.segments = numpy.append(self.segments, (city_arr[-1], city_arr[1]))
        self.length = sum([s.dist for s in self.segments if isinstance(s, Segment)])

    # def __str__(self):
    #     result = '('
    #     for segment in self.segments:
    #         result += '{} '.format(segment.a)
    #     result = result[:-1] + ')'
    #     return result

    # def __repr__(self):
    #     return self.__str__()

    def fitness(self):
        return 1.0 / self.length

    def __lt__(self, other):
        return self.length < other.length

    def __le__(self, other):
        return self.length <= other.length

    def __eq__(self, other):
        return self.segments == other.segments

    def __ne__(self, other):
        return self.segments != other.segments

    def __ge__(self, other):
        return self.length >= other.length

    def __gt__(self, other):
        return self.length > other.length


class Population:
    def __init__(self, cities):
        self.individuals = numpy.array(
            [Circuit(
                numpy.append(
                    numpy.array([cities[0]]),
                    numpy.random.choice(cities_data[1:],
                                        num_cities - 1,
                                        replace=False)))
             for _ in range(options['population_size'])
             ])

    def __str__(self):
        result = '[\n'
        for individual in self.individuals:
            result += '\t{} fitness:{:0.7f} length:{:0.7f}\n'.format(individual,
                                                                     individual.fitness(),
                                                                     individual.length)
        result += ']'
        return result

    def select_parents(self):
        self.individuals.sort()
        self.individuals = self.individuals[:int(options['elite_percent'] / 100 * options['population_size'])]

    def crossover_mutate(self):
        i = random.randint(0, num_cities - 1)
        j = random.randint(i, num_cities)
        for x in range(options['population_size'] - int(options['elite_percent'] / 100 * options['population_size'])):
            a = numpy.random.choice(self.individuals, replace=False)
            b = numpy.random.choice(self.individuals, replace=False)
            result = []
            for y in range(num_cities + 1):
                if i <= y < j:
                    result.append(a[y])
                else:
                    result.append(b[y])
        self.individuals = numpy.append(self.individuals, result)


def get_data(filename):
    with open(filename, 'r') as f:
        global num_cities
        global cities_data
        global start_city
        num_cities = int(f.readline())
        for i in range(num_cities):
            cities_data = numpy.append(cities_data, City(*tuple(map(int, f.readline().strip().split()))))
        start_city = int(f.readline())
        for i, city in enumerate(cities_data):
            if city.number == start_city:
                cities_data[0], cities_data[i] = cities_data[i], cities_data[0]
                break


def main():
    get_options()
    print('''
Running GA on filename {filename}
Options:
    population_size: {pop_size}
    elite_percent: {elite_perc}
    mutation_rate: {mut_rate}
    max_unchanged_generations: {max_unch}
'''.format(filename=options['filename'],
           pop_size=options['population_size'],
           elite_perc=options['elite_percent'],
           mut_rate=options['mutation_rate'],
           max_unch=options['max_unchanged_generations']))

    test_a = City(1, 0, 0)
    test_b = City(2, 4, 3)
    print("{}\n{}".format(test_a, test_b))
    seg_a = Segment(test_a, test_b)
    print(seg_a)
    get_data(options['filename'])
    print(num_cities)
    for city in cities_data:
        print(city)
    print(start_city)
    population = Population(cities_data)
    print(population)
    population.select_parents()
    print('-' * 50)
    print(population)
    population.crossover_mutate()


main()
