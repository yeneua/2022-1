#생선 분류 데이터
import matplotlib.pyplot as plt

bream_length=[25.4, 26.3, 26.5, 29.0, 29.0, 29.7, 29.7, 30.0, 30.0,
30.7, 31.0, 31.0, 31.5, 32.0, 32.0, 32.0, 33.0, 33.0, 33.5, 33.5, 34.0,
34.0, 34.5, 35.0, 35.0, 35.0, 35.0, 36.0, 36.0, 37.0, 38.5, 38.5, 39.5, 41.0, 41.0]
bream_weight=[242, 290, 340, 363, 430, 450, 500, 390, 450, 500, 475, 500,
500, 340, 600, 600, 700, 700, 610, 650, 575, 685, 620, 680, 700, 725, 720,
714, 850, 1000, 920, 955, 925, 975, 950]

plt.scatter(bream_length,bream_weight)
plt.xlabel('length')
plt.ylabel('weight')
plt.show()


print(len(bream_length), len(bream_weight))


#pip install matplotlib
#->Fatal error in launcher: Unable to create process using
#pip error해결 : python -m pip install matplotlib