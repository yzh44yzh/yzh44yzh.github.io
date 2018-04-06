#!/usr/bin/env python3

# TODO
# author page, also active link for this page
# save all pages to files
# check navigation works
# rename index.html to archive.html, season_00.html to index.html, fix navigation
# fix navigation for ./post/*.html pages, links to archive.html instead of index.html

import pystache

model = [
    {
        'input': 'data/prog_cat/season_00.txt',
        'output': 'season_00.html',
        'template': 'data/prog_cat/template.html',
        'bindings': {
            'title': 'Сезон #00.',
            'date': '23 апреля 2016'
        }
    },
    {
        'input': 'data/prog_cat/season_01.txt',
        'output': 'season_01.html',
        'template': 'data/prog_cat/template.html',
        'bindings': {
            'title': 'Сезон #01.',
            'date': '4 августа 2016'
        }
    },
    {
        'input': 'data/prog_cat/season_02.txt',
        'output': 'season_02.html',
        'template': 'data/prog_cat/template.html',
        'bindings': {
            'title': 'Сезон #02.',
            'date': '2 июня 2017'
        }
    },
    {
        'input': 'data/prog_cat/season_03.txt',
        'output': 'season_03.html',
        'template': 'data/prog_cat/template.html',
        'bindings': {
            'title': 'Сезон #03.',
            'date': '24 ноября 2017'
        }
    },
    {
        'input': 'data/prog_cat/season_04.txt',
        'output': 'season_04.html',
        'template': 'data/prog_cat/template.html',
        'bindings': {
            'title': 'Сезон #04.',
            'date': '28 марта 2018'
        }
    }
]

navigation = []
for data in model:
    navigation.append({
        'href': data['output'],
        'text': data['bindings']['title']
        })


def make_page(index, data):
    data['bindings']['episodes'] = get_episodes(data['input'], index)
    for item in navigation:
        item['active'] = item['text'] == data['bindings']['title']
    data['bindings']['navigation'] = navigation
    template = get_template(data['template'])
    page = pystache.render(template, data['bindings'])
    print(page)


def get_episodes(input_file, season_number):
    fd = open(input_file, 'r')
    lines = fd.readlines()
    contents = []
    content = ''
    for line in lines:
        line = line.strip()
        if line:
            content += '<p>' + line[0:-1] + '</p>\n'
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
    return episodes[0:2] # TODO temporary slicing


def get_template(template_file):
    fd = open(template_file, 'r')
    return fd.read()

make_page(4, model[4])
