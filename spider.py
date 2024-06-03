import time
import logging
import pandas as pd
from pypinyin import pinyin, Style
import http.client
from http.client import IncompleteRead,RemoteDisconnected
http.client.HTTPConnection._http_vsn = 10
http.client.HTTPConnection._http_vsn_str = 'HTTP/1.0'
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s:%(message)s')
cities = [
    "北京", "上海", "天津", "重庆", "哈尔滨", "长春", "沈阳", "呼和浩特", "石家庄", "太原",
    "西安", "济南", "乌鲁木齐", "拉萨", "西宁", "兰州", "银川", "郑州", "南京", "武汉",
    "杭州", "合肥", "福州", "南昌", "长沙", "贵阳", "成都", "广州", "昆明", "南宁", "深圳"
]
pinyin_cities = [pinyin(city, style=Style.NORMAL) for city in cities]
s=''
pinyin_cities_str=[]
for pinyin_citie in pinyin_cities:
    for i in pinyin_citie:
        s+=i[0]
    pinyin_cities_str.append(s)
    s=''
# c=input()
# y=input()
# m=input()
print(pinyin_cities_str)
headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36",'Accept': 'gzip, deflate','connection': 'keep-alive','cookie':'__tins__21287555=%7B%22sid%22%3A%201709541865611%2C%20%22vd%22%3A%206%2C%20%22expires%22%3A%201709544665236%7D; __51cke__=; __51laig__=13; ASP.NET_SessionId=gpowp255y2uiaenzrljru4jv; Hm_lvt_f48cedd6a69101030e93d4ef60f48fd0=1709521871; Hm_lpvt_f48cedd6a69101030e93d4ef60f48fd0=1709542865; __gads=ID=755f42511a832e55:T=1709530090:RT=1709542746:S=ALNI_MYW7uinv-3VfZS0d3vO_buwYAoCzQ; __gpi=UID=00000d24a9b50071:T=1709530090:RT=1709542746:S=ALNI_MabRLHW_INREm0Se4qFQGRtAoj7BA; __eoi=ID=d4f33a9c9911eb7d:T=1709530090:RT=1709542746:S=AA-AfjbfwYhiyEoNFIrGqq2q1Zw6'}
for city in pinyin_cities_str:
    for year in range(2014, 2024):
        for month in range(1, 13):
            for i in range(1, 5):
                try:
                    if month<10:
                        url=f'http://www.tianqihoubao.com/aqi/{city}-{year}0{month}.html'
                        print(url)
                        df=pd.read_html(url,encoding='gbk',storage_options=headers)[0]

                        time.sleep(3)
                        if month==1:
                            df.to_csv(f'{year}-{city}-空气质量.csv',mode='a+',index=False,header=False)
                        else:
                            df.iloc[1:,::].to_csv(f'{year}-{city}-空气质量.csv',mode='a+',index=False,header=False)
                        break
                    else:
                        url=f'http://www.tianqihoubao.com/aqi/{city}-{year}{month}.html'
                        print(url)
                        time.sleep(3)
                        df=pd.read_html(url,encoding='gbk',storage_options=headers)[0]
                        df.iloc[1:,::].to_csv(f'{year}-{city}-空气质量.csv',mode='a+',index=False,header=False)
                        break
                except Exception as e:
                    print(e)





