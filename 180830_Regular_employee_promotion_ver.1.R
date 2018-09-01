# 1
# (1)R or Python で comment.csv, meigara_master.csv, score.csvという３つのCSVを読み込んでください。

install.packages("data.table")
library(data.table)

# df_comment <- fread('comment1.csv') これだと文字化け、encoding = 'UTF-8'をつける
df_comment <- fread('comment1.csv', encoding = 'UTF-8')
head(df_comment)

dim(df_comment)

df_meigara_master <- fread('meigara_master1.csv', encoding = 'UTF-8')
head(df_meigara_master)

dim(df_meigara_master)

df_score <- fread('score1.csv', encoding = 'UTF-8')
head(df_score)

dim(df_score)

# (2)銘柄毎のコメント数の合計値をプロットしてください。
# まずは銘柄毎のコメント数の合計値を出す
# meigara名とtextをmeigara_idをキーにして紐づける
install.packages('dplyr')
library(dplyr)

# by = '' クォーテーションマーク忘れてたのでエラー
df1 <- inner_join(df_meigara_master, df_comment, by = 'meigara_id')

# これだとおかしくなるテキスト毎に数が数えられてしまう、まったく同じテキストはないので1,1,1,,,と表示されてしまう
# df2 <- df1 %>% group_by(meigara) %>% count(text)

# countという変数を新たに作成し、n()でデータの数をカウントした値を代入します。 n()でカウント [count]は新しく名付けたい列名
# 「ログデータみたいな数量でないデータはよくこれでカウントしてます。」
# http://www.housecat442.com/?p=346
df_count <- df1%>%
  group_by(meigara)%>%
  summarize(count=n())

df_count

# コメント数の合計値をグラフ化
install.packages("ggplot2")
library(ggplot2) # Rstudioをいったん閉じてから開くとエラーなし

# group_byしたものにはstat = 'identity'を使う
# 文字サイズをelement_text(size=,,)で調整
ggplot(data = df_count) +
  geom_bar(aes(meigara, count), stat = 'identity') +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1))

# グラフのプロットまではここで完了。ここからはデータ量を減らし見栄えを良くする


# mean以上の合計値をプロットしてみる
# まずはmean,medianを出してみる
df_count %>% 
  summarise(mean(count), median(count))
# mean = 15.7, median = 11.0 

# mean = 15.7→16以上のcountがある銘柄を抽出
df_count_overmean <- df_count %>% 
  filter(count > 16) 
## 何も抽出されない、、なぜ→filter(mean(count)>16)としてしまっていたから
## これだと200の銘柄を平均という1つの値にまとめられてしまうため当然最大1つしか抽出されないので注意

# 行数、列数を確認
dim(df_count_overmean)

# 最小値を確認し、17以上の値が抽出できているかどうかを見る
df_count_overmean %>% 
  summarise(min(count))
# minが17なので正しく抽出できていると判断

# グラフ化 文字サイズをelement_text(size=,,)で調整
ggplot(data = df_count_overmean) +
  geom_bar(aes(meigara, count), stat = 'identity') +
  theme_classic() +
  theme(axis.text.x = element_text(size= 10, angle = 90, hjust = 1))


# ここからはmean = 16以下のcountの銘柄を抽出
df_count_undermean <- df_count %>% 
  filter(count <= 16)

# 行数、列数を確認
dim(df_count_undermean)

# 最大値を確認し、16以下の値が抽出されているかどうかを見る
df_count_undermean %>% 
  summarise(max(count))

# グラフ化 文字サイズをelement_text(size=,,)で調整
ggplot(data = df_count_undermean) +
  geom_bar(aes(meigara, count), stat = 'identity') +
  theme_classic() +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1))
