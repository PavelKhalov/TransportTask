#include <iostream>
#include <vector>
#include <algorithm>
#include <iomanip>
#include <cmath>
#include <queue>
#include <climits>
#include <unordered_set>
#include <cfloat>
#include <functional>
#include <stack>
using namespace std;

const double EPS = 1e-9;

void printMatrix(const vector<vector<double>>& matrix, const string& title) {
    cout << title << ":\n";
    for (const auto& row : matrix) {
        for (double val : row) {
            if (val == DBL_MAX) cout << setw(10) << "INF" << " ";
            else if (fabs(val) < EPS) cout << setw(10) << "0" << " ";
            else cout << setw(10) << val << " ";
        }
        cout << "\n";
    }
    cout << endl;
}

void printVector(const vector<double>& vec, const string& title) {
    cout << title << ": ";
    for (double val : vec) {
        if (val == DBL_MAX) cout << "INF ";
        else cout << val << " ";
    }
    cout << "\n\n";
}

void printBasis(const vector<vector<bool>>& basis, const string& title) {
    cout << title << ":\n";
    for (const auto& row : basis) {
        for (bool val : row) {
            cout << (val ? 'B' : '.') << " ";
        }
        cout << "\n";
    }
    cout << endl;
}

void printCycle(const vector<pair<int, int>>& cycle) {
    cout << "Цикл перераспределения: ";
    for (const auto& p : cycle) {
        cout << "(" << p.first << "," << p.second << ") ";
    }
    cout << "\n\n";
}

void balanceProblem(vector<double>& supply, vector<double>& demand, vector<vector<double>>& costs) {
    double totalSupply = 0, totalDemand = 0;
    for (double s : supply) totalSupply += s;
    for (double d : demand) totalDemand += d;

    if (fabs(totalSupply - totalDemand) < EPS) return;

    cout << "Балансировка задачи:\n";
    cout << "Суммарный запас: " << totalSupply << ", суммарная потребность: " << totalDemand << "\n";

    if (totalSupply > totalDemand) {
        cout << "Добавляем фиктивного потребителя с потребностью: " << totalSupply - totalDemand << "\n\n";
        demand.push_back(totalSupply - totalDemand);
        for (vector<double>& row : costs)
            row.push_back(0);
    }
    else {
        cout << "Добавляем фиктивного поставщика с запасом: " << totalDemand - totalSupply << "\n\n";
        supply.push_back(totalDemand - totalSupply);
        costs.push_back(vector<double>(demand.size(), 0));
    }
}

pair<int, int> findMinElement(const vector<vector<double>>& costs,
    const vector<bool>& rowDone,
    const vector<bool>& colDone) {
    double minCost = DBL_MAX;
    pair<int, int> minPos = { -1, -1 };

    for (int i = 0; i < costs.size(); ++i) {
        if (rowDone[i]) continue;
        for (int j = 0; j < costs[0].size(); ++j) {
            if (colDone[j]) continue;
            if (costs[i][j] < minCost) {
                minCost = costs[i][j];
                minPos = { i, j };
            }
        }
    }
    return minPos;
}

vector<vector<double>> minCostMethod(vector<double> supply,
    vector<double> demand,
    const vector<vector<double>>& costs,
    vector<vector<bool>>& basis) {
    int m = supply.size();
    int n = demand.size();
    vector<vector<double>> plan(m, vector<double>(n, 0));
    basis.assign(m, vector<bool>(n, false));

    vector<bool> rowDone(m, false);
    vector<bool> colDone(n, false);
    int doneCount = 0;

    cout << "Построение начального плана методом минимального элемента:\n";
    int step = 1;

    while (doneCount < m + n - 1) {
        pair<int, int> pos = findMinElement(costs, rowDone, colDone);
        int i = pos.first;
        int j = pos.second;
        if (i == -1 || j == -1) break;

        double amount = min(supply[i], demand[j]);
        plan[i][j] = amount;
        basis[i][j] = true;

        cout << "Шаг " << step++ << ": ";
        cout << "Поставщик " << i << " -> Потребитель " << j << ". " << amount << " единиц\n";

        supply[i] -= amount;
        demand[j] -= amount;

        if (fabs(supply[i]) < EPS && !rowDone[i]) {
            rowDone[i] = true;
            doneCount++;
        }
        if (fabs(demand[j]) < EPS && !colDone[j]) {
            colDone[j] = true;
            doneCount++;
        }
    }
    cout << "\nНачальный план перевозок:\n";
    printMatrix(plan, "План");
    return plan;
}

void addDegenerateCases(vector<vector<bool>>& basis,
    vector<vector<double>>& plan,
    const vector<double>& supply,
    const vector<double>& demand) {
    int m = basis.size();
    int n = basis[0].size();
    int basisCount = 0;
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < n; ++j)
            if (basis[i][j]) basisCount++;

    int required = m + n - 1;
    if (basisCount >= required) return;

    cout << "Добавление базисных нулей (вырожденный случай):\n";
    cout << "Текущее количество базисных клеток: " << basisCount << ", требуется: " << required << "\n";

    vector<int> parent(m + n);
    for (int i = 0; i < m + n; ++i) parent[i] = i;

    function<int(int)> find = [&](int x) {
        if (parent[x] != x) parent[x] = find(parent[x]);
        return parent[x];
        };

    auto unite = [&](int x, int y) {
        int rx = find(x), ry = find(y);
        if (rx != ry) parent[ry] = rx;
        };

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (basis[i][j]) {
                if (find(i) != find(m + j)) {
                    unite(i, m + j);
                }
            }
        }
    }

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (!basis[i][j] && find(i) != find(m + j)) {
                basis[i][j] = true;
                plan[i][j] = 0;
                unite(i, m + j);
                cout << "Добавлен базисный ноль в клетке (" << i << "," << j << ")\n";
                basisCount++;
                if (basisCount == required) {
                    printBasis(basis, "Базис после добавления нулей");
                    return;
                }
            }
        }
    }
}

void calculatePotentials(const vector<vector<double>>& costs,
    const vector<vector<bool>>& basis,
    vector<double>& u,
    vector<double>& v) {
    int m = costs.size();
    int n = costs[0].size();
    u.assign(m, DBL_MAX);
    v.assign(n, DBL_MAX);
    u[0] = 0;

    queue<pair<int, char>> q;
    q.push(make_pair(0, 'u'));

    while (!q.empty()) {
        pair<int, char> node = q.front();
        q.pop();
        int idx = node.first;
        char type = node.second;

        if (type == 'u') {
            for (int j = 0; j < n; ++j) {
                if (basis[idx][j] && v[j] == DBL_MAX) {
                    v[j] = costs[idx][j] - u[idx];
                    q.push(make_pair(j, 'v'));
                }
            }
        }
        else {
            for (int i = 0; i < m; ++i) {
                if (basis[i][idx] && u[i] == DBL_MAX) {
                    u[i] = costs[i][idx] - v[idx];
                    q.push(make_pair(i, 'u'));
                }
            }
        }
    }

    printVector(u, "Потенциалы U");
    printVector(v, "Потенциалы V");
}

// Улучшенный поиск цикла с использованием BFS
vector<pair<int, int>> findCycle(int i0, int j0, const vector<vector<bool>>& basis) {
    int m = basis.size();
    int n = basis[0].size();

    vector<int> rowParent(m, -1);
    vector<int> colParent(n, -1);
    queue<pair<char, int>> q;

    q.push(make_pair('r', i0));
    rowParent[i0] = -2; // mark start

    bool found = false;
    while (!q.empty() && !found) {
        char type = q.front().first;
        int idx = q.front().second;
        q.pop();

        if (type == 'r') {
            int i = idx;
            for (int j = 0; j < n; j++) {
                if (basis[i][j] && colParent[j] == -1) {
                    colParent[j] = i;
                    if (j == j0) {
                        found = true;
                        break;
                    }
                    q.push(make_pair('c', j));
                }
            }
        }
        else if (type == 'c') {
            int j = idx;
            for (int i = 0; i < m; i++) {
                if (basis[i][j] && rowParent[i] == -1) {
                    rowParent[i] = j;
                    q.push(make_pair('r', i));
                }
            }
        }
    }

    if (!found) {
        cout << "Цикл не найден!" << endl;
        return {};
    }

    vector<pair<int, int>> cycle;
    cycle.push_back({ i0, j0 });

    int j = j0;
    while (true) {
        int i = colParent[j];
        if (i == -2) break; // reached start
        cycle.push_back({ i, j });
        if (i == i0) break; // safety break

        j = rowParent[i];
        if (j == -1) break;
        cycle.push_back({ i, j });
    }

    return cycle;
}

void redistribute(vector<vector<double>>& plan,
    const vector<pair<int, int>>& cycle) {
    if (cycle.empty()) {
        cout << "Ошибка: пустой цикл!" << endl;
        return;
    }

    double minAmount = DBL_MAX;
    // Ищем минимальное значение в нечетных позициях цикла (начиная с 1)
    for (int k = 1; k < cycle.size(); k += 2) {
        int i = cycle[k].first;
        int j = cycle[k].second;
        if (plan[i][j] < minAmount) {
            minAmount = plan[i][j];
        }
    }

    cout << "Минимальное значение для перераспределения: " << minAmount << "\n";
    cout << "Изменения в цикле:\n";

    for (int k = 0; k < cycle.size(); ++k) {
        int i = cycle[k].first;
        int j = cycle[k].second;
        double oldVal = plan[i][j];

        if (k % 2 == 0) { // Четные позиции: увеличение
            plan[i][j] += minAmount;
            cout << "+ " << minAmount << " в (" << i << "," << j << "): "
                << oldVal << " -> " << plan[i][j] << "\n";
        }
        else { // Нечетные позиции: уменьшение
            plan[i][j] -= minAmount;
            cout << "- " << minAmount << " в (" << i << "," << j << "): "
                << oldVal << " -> " << plan[i][j] << "\n";
        }
    }
    cout << endl;
}

void updateBasis(vector<vector<bool>>& basis,
    vector<vector<double>>& plan,
    const vector<pair<int, int>>& cycle) {
    int m = plan.size();
    int n = plan[0].size();

    // Добавляем новую базисную клетку (начало цикла)
    int i0 = cycle[0].first;
    int j0 = cycle[0].second;
    basis[i0][j0] = true;
    cout << "Добавлена в базис клетка (" << i0 << "," << j0 << ")\n";

    // Ищем клетку для удаления (минимальное значение в нечетных позициях)
    bool removed = false;
    for (int k = 1; k < cycle.size(); k += 2) {
        int i = cycle[k].first;
        int j = cycle[k].second;
        if (fabs(plan[i][j]) < EPS) {
            basis[i][j] = false;
            cout << "Удалена из базиса клетка (" << i << "," << j << ")\n";
            removed = true;
            break;
        }
    }

    // Если не удалось найти нулевую клетку, удаляем первую с минимальным значением
    if (!removed) {
        double minVal = DBL_MAX;
        pair<int, int> minCell;

        for (int k = 1; k < cycle.size(); k += 2) {
            int i = cycle[k].first;
            int j = cycle[k].second;
            if (plan[i][j] < minVal) {
                minVal = plan[i][j];
                minCell = { i, j };
            }
        }

        basis[minCell.first][minCell.second] = false;
        cout << "Удалена из базиса клетка (" << minCell.first << "," << minCell.second
            << ") со значением " << minVal << "\n";
    }

    printBasis(basis, "Обновленный базис");
}

double calculateTotalCost(const vector<vector<double>>& plan,
    const vector<vector<double>>& costs) {
    double total = 0;
    for (int i = 0; i < plan.size(); ++i) {
        for (int j = 0; j < plan[0].size(); ++j) {
            total += plan[i][j] * costs[i][j];
        }
    }
    return total;
}

vector<vector<double>> solveTransportProblem(vector<double> supply,
    vector<double> demand,
    vector<vector<double>> costs) {
    cout << "===== НАЧАЛО РЕШЕНИЯ ТРАНСПОРТНОЙ ЗАДАЧИ =====\n\n";
    cout << "Исходные данные:\n";
    printVector(supply, "Запасы");
    printVector(demand, "Потребности");
    printMatrix(costs, "Матрица стоимостей");

    balanceProblem(supply, demand, costs);
    printVector(supply, "Запасы после балансировки");
    printVector(demand, "Потребности после балансировки");
    printMatrix(costs, "Матрица стоимостей после балансировки");

    vector<vector<bool>> basis;
    vector<vector<double>> plan = minCostMethod(supply, demand, costs, basis);
    printBasis(basis, "Базис начального плана");

    addDegenerateCases(basis, plan, supply, demand);

    bool optimal = false;
    int iteration = 1;

    while (!optimal) {
        cout << "===== ИТЕРАЦИЯ " << iteration++ << " =====\n";
        vector<double> u, v;
        calculatePotentials(costs, basis, u, v);

        optimal = true;
        int i0 = -1, j0 = -1;
        double minDelta = 0;

        cout << "Оценки для свободных клеток:\n";
        vector<vector<double>> deltas(costs.size(), vector<double>(costs[0].size(), DBL_MAX));

        for (int i = 0; i < costs.size(); ++i) {
            for (int j = 0; j < costs[0].size(); ++j) {
                if (basis[i][j]) {
                    deltas[i][j] = DBL_MAX;
                    continue;
                }
                if (u[i] == DBL_MAX || v[j] == DBL_MAX) {
                    deltas[i][j] = DBL_MAX;
                    cout << "Клетка (" << i << "," << j << "): потенциалы не определены\n";
                    continue;
                }
                double delta = costs[i][j] - (u[i] + v[j]);
                deltas[i][j] = delta;
                cout << "Клетка (" << i << "," << j << "): " << delta << "\n";

                if (delta < minDelta - EPS) {
                    minDelta = delta;
                    i0 = i;
                    j0 = j;
                    optimal = false;
                }
            }
        }
        printMatrix(deltas, "Матрица оценок");

        if (optimal) {
            cout << "Оптимальное решение достигнуто!\n";
            break;
        }

        cout << "Наименьшая отрицательная оценка: " << minDelta
            << " в клетке (" << i0 << "," << j0 << ")\n";

        vector<pair<int, int>> cycle = findCycle(i0, j0, basis);
        if (cycle.empty()) {
            cout << "Цикл не найден! Прерывание оптимизации.\n";
            break;
        }
        printCycle(cycle);

        redistribute(plan, cycle);
        printMatrix(plan, "План после перераспределения");

        updateBasis(basis, plan, cycle);
        printMatrix(plan, "Обновленный план перевозок");

        double totalCost = calculateTotalCost(plan, costs);
        cout << "Текущая стоимость: " << totalCost << "\n\n";
    }

    cout << "===== РЕШЕНИЕ ЗАВЕРШЕНО =====\n\n";
    return plan;
}

int main() {
    setlocale(LC_ALL, "rus");
    int m, n;
    cout << "Введите количество поставщиков: ";
    cin >> m;
    cout << "Введите количество потребителей: ";
    cin >> n;

    vector<double> supply(m);
    vector<double> demand(n);
    vector<vector<double>> costs(m, vector<double>(n));

    cout << "Введите запасы поставщиков:\n";
    for (int i = 0; i < m; ++i) {
        cin >> supply[i];
    }

    cout << "Введите потребности потребителей:\n";
    for (int i = 0; i < n; ++i) {
        cin >> demand[i];
    }

    cout << "Введите матрицу стоимостей:\n";
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            cin >> costs[i][j];
        }
    }

    vector<vector<double>> solution = solveTransportProblem(supply, demand, costs);

    cout << "\nОптимальный план перевозок:\n";
    for (int i = 0; i < solution.size(); ++i) {
        for (int j = 0; j < solution[i].size(); ++j) {
            cout << setw(10) << solution[i][j] << " ";
        }
        cout << "\n";
    }

    double totalCost = calculateTotalCost(solution, costs);
    cout << "\nОбщая стоимость перевозок: " << totalCost << endl;

    return 0;
}