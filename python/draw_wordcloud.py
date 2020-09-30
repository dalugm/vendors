#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
usage: txtmerge.py <text> <font> <stopwords> <userdict> [bg_image]
'''

from __future__ import print_function

import sys
import jieba.analyse
import matplotlib.pyplot as plt
from wordcloud import WordCloud

assert len(sys.argv) >= 2, \
    "\nUsage: draw_wordcloud.py <text> <font> <stopwords> <userdict> \
[bg_image](can be omitted)"


text_path = str(sys.argv[1])
font_path = str(sys.argv[2])
stopwords_path = str(sys.argv[3])
jieba.load_userdict(str(sys.argv[4]))
if len(sys.argv) == 6:
    bg_image_path = str(sys.argv[5])
else:
    bg_image_path = ''


def clean_using_stopword(text):
    """
    去除停顿词，利用常见停顿词表+自建词库
    :param text:
    :return:
    """
    mywordlist = []
    # 用精确模式来分词
    seg_list = jieba.cut(text, cut_all=False)
    liststr = "/ ".join(seg_list)
    with open(stopwords_path) as f_stop:
        f_stop_text = f_stop.read()
        f_stop_seg_list = f_stop_text.split('\n')
    for myword in liststr.split('/'):  # 去除停顿词，生成新文档
        if not (myword.strip() in f_stop_seg_list) and len(myword.strip()) > 1:
            mywordlist.append(myword)
    return ''.join(mywordlist)


def preprocessing():
    """
    文本预处理
    :return:
    """
    with open(text_path) as f:
        content = f.read()
    return clean_using_stopword(content)
    return content


# 提取关键词
def extract_keywords():
    """
    利用jieba来进行中文分词。
    analyse.extract_tags采用TF-IDF算法进行关键词的提取。
    :return:
    """
    # 抽取1000个关键词，带权重，后面需要根据权重来生成词云
    tags = jieba.analyse.extract_tags(preprocessing(), 1000, withWeight=True)
    keywords = dict()
    for i in tags:
        print("%s---%f" % (i[0], i[1]))
        keywords[i[0]] = i[1]
    return keywords


if len(bg_image_path) > 0:
    def draw_wordcloud():
        """
        生成词云。1.配置WordCloud。2.plt进行显示
        :return:
        """
        back_coloring = plt.imread(bg_image_path)
        wc = WordCloud(font_path=font_path,      # 设置字体
                       background_color="white",  # 背景颜色
                       max_words=2000,           # 词云显示的最大词数
                       mask=back_coloring,        # 填充图
                       scale=2,
                       width=1680,
                       height=1050,
                       )
        # 根据频率生成词云
        wc.generate_from_frequencies(extract_keywords())
        # 显示图片
        plt.figure()
        plt.imshow(wc)
        plt.axis("off")
        plt.show()
        # 保存到本地
        wc.to_file("wordcloud.jpg")
else:
    def draw_wordcloud():
        """
        生成词云。1.配置WordCloud。2.plt进行显示
        :return:
        """
        wc = WordCloud(font_path=font_path,      # 设置字体
                       background_color="white",  # 背景颜色
                       max_words=2000,           # 词云显示的最大词数
                       scale=2,
                       width=1680,
                       height=1050,
                       )
        # 根据频率生成词云
        wc.generate_from_frequencies(extract_keywords())
        # 显示图片
        plt.figure()
        plt.imshow(wc)
        plt.axis("off")
        plt.show()
        # 保存到本地
        wc.to_file("wordcloud.jpg")


if __name__ == '__main__':
    draw_wordcloud()
