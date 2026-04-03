# Hazard Results Interpretation

这份文档总结当前 hazard 主模型与敏感性分析的主要结果，并给出适合论文、基金和汇报的解释口径。

## 1. 本轮分析口径

- 风险集：`species × province × year`
- SDM 候选集：`birdwatch_2002_2025 + rescue_1980_2025_gbif`
- 主模型阈值：`100`
- 敏感性阈值：`50`、`200`
- 时间窗口：`2002–2024`
- 当前主模型不纳入降水
- 当前主模型仅使用温度梯度、记录型 effort 及其交互
- 当前主模型不纳入 observer effort

## 2. 数据覆盖

主模型测试版实际覆盖：

- 新纪录窗口总数：`962`
- 成功接入主模型的新纪录：`526`
- 窗口物种数：`534`
- 成功接入主模型的物种：`333`
- 全风险集行数：`177,724`
- 完整案例建模行数：`13,058`
- 完整案例首次事件数：`517`

如果需要追踪筛选过程，建议直接查看：

- `diagnostics/support_filter_trace.csv`

当前阈值 `100` 下的支持集收窄过程是：

- `window`: `962` 条记录，`534` 个物种
- `after_sdm`: `671` 条记录，`390` 个物种
- `after_native`: `526` 条记录，`333` 个物种
- `after_effort`: `526` 条记录，`333` 个物种

## 3. 模型比较

跨 `50 / 100 / 200` 三个阈值，核心结论一致：

- `M4` 的 AIC 最低
- `M0–M3` 均明显差于 `M4`

这说明：

> 仅靠年份基线、仅靠气候主效应、或仅靠 effort 主效应，都不能像“气候 × effort 交互模型”那样解释首次新纪录风险。

## 4. 核心系数解释

### 4.1 温度梯度 × effort 交互项

在 `M4` 中：

- `temp_grad_z:log_effort_record_z = 0.208`
- `p = 4.68e-6`
- `HR = 1.23`
- `95% CI = 1.13–1.35`

解释：

> 当省份调查记录 effort 更高时，温度梯度对应的新纪录风险上升更明显。

这正是“可见性门槛”机制的实证信号：

> 气候变化可能已经推动潜在扩张，但只有在调查努力足够高的地区，这种扩张更容易以“首次新纪录”的形式被观察到。

### 4.2 温度梯度主效应

在 `M4` 中：

- `temp_grad_z = -0.143`
- `p = 0.0069`
- `HR = 0.87`

这不应被简单解释为“升温抑制新纪录”。
更稳妥的解释是：

> 在包含交互项后，温度梯度主效应反映的是“在 effort 处于参考水平时”的条件效应，而不是对所有努力水平下的平均总效应。

因此，这个负主效应并不否定气候扩张机制；真正有生物学解释力的是交互项。

### 4.3 effort 主效应

当前结果中：

- `log_effort_record_z` 为负但不显著

这表明：

> 在把温度梯度与交互项纳入后，record effort 的解释并不是简单线性增加首次新纪录风险，而更像是对气候信号的放大器。

## 5. 阈值敏感性分析

在 `50 / 100 / 200` 三个阈值下：

- 最优模型都为 `M4`
- `temp_grad_z:log_effort_record_z` 始终保持显著正向
- 数据覆盖规模会随阈值变化，但主推断不变

这说明本轮分析对 SDM 阈值选择表现出很强稳健性。

适合在文中写成：

> The interaction between temperature gradient and record-based survey effort remained stable across alternative SDM province thresholds, indicating that the main inference was not sensitive to threshold choice within the tested range.

## 6. 当前最推荐的结果表述

中文：

> 在基于 SDM 约束的省级风险集中，离散时间 hazard 模型显示，最佳模型为包含温度梯度、调查努力及其交互项的共同驱动模型。温度梯度与记录型调查努力之间的正向交互显著且在不同 SDM 阈值下稳健，表明气候变化带来的潜在分布扩张更容易在高调查强度地区被记录为新的省级分布纪录。

英文：

> In the SDM-constrained province-level risk set, the best-supported discrete-time hazard model was the joint driver model including temperature gradient, survey effort, and their interaction. The positive interaction between temperature gradient and record-based survey effort was strong and stable across SDM thresholds, indicating that climate-associated range expansion was more likely to become visible as new provincial records in better-surveyed regions.

## 7. 仍需保留的限制说明

当前版本仍有两个限制需要在正式稿中说明：

1. `province-year climate` 面板仍是基于已有事件层气候结果补齐的测试版输入，而不是完整独立生成的全年度省级面板。
2. 迁徙型等物种性状尚未在当前主模型中稳定纳入，因此本轮重点解释的是“温度 × record effort”机制，而非性状调节机制。

## 8. 下一步建议

最优先：

1. 在服务器上重跑一遍完整 bundle，确认与本地一致。
2. 用正式 `province-year annual climate panel` 替换当前测试版 climate 面板。

随后：

3. 再引入 `mig` 与其他关键 traits 做扩展模型。
4. 将当前汇总表与森林图、山脊图直接接入正文与补充材料。
