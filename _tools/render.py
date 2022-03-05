import os
import chevron  # https://github.com/noahmorrison/chevron


def render_all():
    base_tpl_file = open('_templates/base.tpl.html')
    pages = [
        ('index', 'Главная')
    ]
    for page in pages:
        render_page(page, base_tpl_file)


def render_page(page, base_tpl_file):
    print(f'Render {page}')
    (page, page_name) = page
    page_tpl_file = open(f'_templates/{page}.tpl.html')
    content = page_tpl_file.read()
    data = {
        'page_name': page_name,
        'content': content
    }
    content = chevron.render(base_tpl_file, data)
    page_file = open(f'{page}.html', 'w')
    page_file.write(content)

    
if __name__ == '__main__':
    render_all()

