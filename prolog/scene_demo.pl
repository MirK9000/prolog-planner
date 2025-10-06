% Комната 6000 x 4000 мм
rects:room_size(6000, 4000).

% Политика (можно не задавать, т.к. дефолт уже 1200)
% zones:policy(min_aisle_width_mm, 1200).

% Одна дверь на западной стене, ширина 1000 мм, смещена на 1500 мм
zones:door(d1, side(west), offset(1500), width(1000)).

% Две колонны (запретные зоны для мебели И препятствия для ходьбы)
zones:forbidden_zone(col1, column, rect(2500, 1500, 400, 400)).
zones:forbidden_zone(col2, column, rect(4200, 2600, 400, 400)).
