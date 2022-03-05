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
        ('courses', 'Учебные курсы'),
        ('blog', 'Блог'),
        ('prog_cats', 'Коты-программисты'),
        ('music', 'Музыка'),
    ]
    for page in pages:
        data = {
            'navigation': build_navigation(pages, page)
        }
        render_page(page, base_tpl, data)


def build_navigation(pages, current_page):
    res = ''
    for page in pages:
        if page == current_page:
            (_, page_name) = page
            res += f'<td>{page_name}</td>\n'
        else:
            (page_id, page_name) = page
            res += f'<td><a href="/{page_id}.html">{page_name}</a></td>\n'
    return res


def render_page(page, base_tpl, data):
    print(f'Render {page}')
    (page_id, page_name) = page
    page_tpl_file = open(f'_templates/{page_id}.tpl.html')
    content = page_tpl_file.read()
    page_tpl_file.close()
    data['page_name'] = page_name
    data['content'] = content
    content = chevron.render(base_tpl, data)
    page_file = open(f'{page_id}.html', 'w')
    page_file.write(content)
    page_file.close()

    
if __name__ == '__main__':
    render_all()

