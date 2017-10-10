# Q. サイコロを作ろう！！  
# import numpy as np # arange用
import random #乱数発生用
import matplotlib.pyplot as plt #ヒストグラム用



# サイコロオブジェクト Diceの定義
# Dice は6面のサイコロ， randomライブラリをimport


class Dice:
    def shoot(self):
        return random.randint(1,6)
    
sai = Dice() #Diceクラスのインスタンス化

sai.shoot() #メソッドの呼び出し

# サイコロを五回振ってみよう
x=[]
for i in range(10): # 10回繰り返し．rrangeで指定
    x.append(sai.shoot()) #list用メソッドappendを使う
    # 単純な記録はこのコードが速いらしい
x #結果の呼び出し



# faceを引数にした　x面体サイコロ

class Dice:
    def __init__(self, face):
        self.face = face
        print("This dice has",face,"faces" )
    def shoot(self):
        return random.randint(1, self.face)
    
sai10 = Dice(10) # 10面体サイコロ

sai10.shoot()

x=[]
for i in range(20):
    x.append(sai10.shoot())
x

# サイコロ振りの可視化
# サイコロ10000回振り
x=[]
for i in range(10000):
    x.append(sai10.shoot())
# ヒストグラムを出力
plt.hist(x)




