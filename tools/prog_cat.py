#!/usr/bin/env python3

# TODO
# - rename index.html to archive.html, season_00.html to index.html, check navigation again
# - fix navigation for ./post/*.html pages, links to archive.html instead of index.html

import pystache

model = [
    {
        'input': 'data/prog_cat/season_00.txt',
        'output': 'season_00.html', # TODO index.html
        'bindings': {
            'title': 'Сезон #00',
            'date': '23 апреля 2016',
            'author_page': False
        }
    },
    {
        'input': 'data/prog_cat/season_01.txt',
        'output': 'season_01.html',
        'bindings': {
            'title': 'Сезон #01',
            'date': '4 августа 2016',
            'author_page': False
        }
    },
    {
        'input': 'data/prog_cat/season_02.txt',
        'output': 'season_02.html',
        'bindings': {
            'title': 'Сезон #02',
            'date': '2 июня 2017',
            'author_page': False
        }
    },
    {
        'input': 'data/prog_cat/season_03.txt',
        'output': 'season_03.html',
        'bindings': {
            'title': 'Сезон #03',
            'date': '24 ноября 2017',
            'author_page': False
        }
    },
    {
        'input': 'data/prog_cat/season_04.txt',
        'output': 'season_04.html',
        'bindings': {
            'title': 'Сезон #04',
            'date': '28 марта 2018',
            'author_page': False
        }
    },
    {
        'input': 'data/prog_cat/author.html',
        'output': 'author.html',
        'bindings': {
            'title': 'Авторы',
            'author_page': True
        }
    }
]

navigation = []
for data in model[0:-1]:
    navigation.append({
        'href': data['output'],
        'text': data['bindings']['title']
        })


def make_page(index, data):
    if data['bindings']['author_page']:
        data['bindings']['author_content'] = get_file(data['input'])
    else:
        data['bindings']['episodes'] = get_episodes(data['input'], index)

    for item in navigation:
        item['active'] = item['text'] == data['bindings']['title']
    data['bindings']['navigation'] = navigation
    content = pystache.render(template, data['bindings'])
    save_file(data['output'], content)


def get_episodes(input_file, season_number):
    fd = open(input_file, 'r')
    lines = fd.readlines()
    contents = []
    content = ''
    for line in lines:
        line = line.strip()
        if line:
            content += '<p>' + line + '</p>\n'
        else:
            if content:
                contents.append(content)
                content = ''
    episodes = []
    for i, content in enumerate(contents):
        episodes.append({
            'e_title': 'Эпизод #{:02X}'.format(i + season_number * 16),
            'e_content': content
        })
    return episodes


def get_file(file):
    fd = open(file, 'r')
    return fd.read()


def save_file(file, content):
    print("save {}".format(file))
    fd = open(file, 'w')
    fd.write(content)


template = get_file('data/prog_cat/template.html')

for i, item in enumerate(model):
    make_page(i, item)
