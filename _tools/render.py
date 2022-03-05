# https://github.com/noahmorrison/chevron
# https://mustache.github.io/mustache.5.html
# https://mustache.github.io/mustache.1.html

import os
import chevron

def render_all():
    base_tpl_file = open('_templates/base.tpl.html')
    base_tpl = base_tpl_file.read()
    base_tpl_file.close()
    
    pages = [
        ('index', 'Главная'),
        ('courses', 'Учебные курсы')
    ]
    for page in pages:
        render_page(page, base_tpl)


def render_page(page, base_tpl):
    print(f'Render {page}')
    (page, page_name) = page
    page_tpl_file = open(f'_templates/{page}.tpl.html')
    content = page_tpl_file.read()
    page_tpl_file.close()
    data = {
        'page_name': page_name,
        'content': content
    }
    content = chevron.render(base_tpl, data)
    page_file = open(f'{page}.html', 'w')
    page_file.write(content)
    page_file.close()

    
if __name__ == '__main__':
    render_all()

