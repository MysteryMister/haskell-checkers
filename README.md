# Игра "шашки"

### Описание проекта
Настольная игра "шашки", реализованная на языке haskell. Учебный пет-проект.

##### Функционал
1. режим игры - 1v1 PvP с использованием GUI;
2. невозможные ходы блокируются программой;
3. победа/ничья/поражение определяется автоматически путем проверки множества условий.

##### Управление
- ЛКМ - выбор фигуры/замена выбранной фигуры/совершение одной итерации;
- ПРОБЕЛ - перезапуск игры;
- ESC - выход из игры.

##### GUI
Рассмотрим несколько сценариев игры.
1. Начало партии.
![1](https://github.com/MysteryMister/haskell-checkers/assets/24231731/1703b04a-18ff-4e00-918a-ff80fc41d7a0)

2. "Белые" прорвались сквозь оборону "черных" и заполучили свою первую дамку. Зеленой обводкой выделена фигура - текущий выбор игрока для хода.
![2](https://github.com/MysteryMister/haskell-checkers/assets/24231731/6faec65c-effd-48c2-b898-0c778097d854)

3. Победа "черных".
![3](https://github.com/MysteryMister/haskell-checkers/assets/24231731/f604de52-aaea-4a82-932e-8306242a2f6b)

### Используемые библиотеки
- **Gloss** – графический интерфейс и обработка внешних событий.

### Модули проекта
- **Checkers.hs** – обработка нажатий пользователя; определение, на какую фигуру он нажал; инициализация игры;
- **Logic.hs** – построения списка возможных ходов для фигур;
- **Types.hs** – описание типов данных;
- **Constants.hs** – описание констант;
- **Visuals.hs** – отрисовка доски, фигур и текстовой информации игрокам;
- **ConfigBoard.hs** – обновление содержимого игровой доски и всевозможные вспомогательные функции, работающие с доской;
- **GameFlow.hs** – завершение хода и итерации, проверка на поражение и ничью.

### Запуск программы
Из корневой папки выполнить команду: 

	stack run
