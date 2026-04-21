const fs = require('fs');
const path = require('path');
const PptxGenJS = require('pptxgenjs');

// ============================================================
// Build SVG-based editable PPTX files for two bird maps
// 为两张鸟类地图构建基于 SVG 的可编辑 PPTX
// ============================================================
//
// Purpose / 目标
// 1. Place pre-generated SVG main maps and inset maps into PowerPoint slides.
// 2. Preserve vector quality for downstream editing in PowerPoint.
// 3. Output two separate PPTX files and mirror them to the output folder.
//
// Notes / 说明
// SVG content is embedded as vector artwork. In modern PowerPoint, it can often
// be converted to shapes or ungrouped for further editing, which is usually more
// editable than raster-image exports.
// ============================================================

const repoTaskDir = '/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_spatiotemporal_patterns';
const mirrorTaskDir = '/Users/dingchenchen/Documents/New records/bird_new_records_R_output/tasks/bird_spatiotemporal_patterns';
const figuresDir = path.join(repoTaskDir, 'figures');
const mirrorFiguresDir = path.join(mirrorTaskDir, 'figures');

const slideW = 13.333;
const slideH = 7.5;
const insetLeft = 0.842;
const insetBottom = 0.0005;
const insetRight = 0.998;
const insetTop = 0.232;

function assertExists(filePath) {
  if (!fs.existsSync(filePath)) {
    throw new Error(`Required file not found: ${filePath}`);
  }
}

async function buildOne(mainSvg, insetSvg, outPptx) {
  assertExists(mainSvg);
  assertExists(insetSvg);

  const pptx = new PptxGenJS();
  pptx.layout = 'LAYOUT_WIDE';
  pptx.author = 'Codex';
  pptx.company = 'OpenAI';
  pptx.subject = 'Bird new-record editable map export';
  pptx.title = path.basename(outPptx, '.pptx');
  pptx.lang = 'en-US';
  pptx.theme = {
    headFontFace: 'Arial',
    bodyFontFace: 'Arial',
    lang: 'en-US'
  };

  const slide = pptx.addSlide();
  slide.background = { color: 'FFFFFF' };

  slide.addImage({
    path: mainSvg,
    x: 0,
    y: 0,
    w: slideW,
    h: slideH
  });

  slide.addImage({
    path: insetSvg,
    x: slideW * insetLeft,
    y: slideH * (1 - insetTop),
    w: slideW * (insetRight - insetLeft),
    h: slideH * (insetTop - insetBottom)
  });

  await pptx.writeFile({ fileName: outPptx });
  fs.copyFileSync(outPptx, path.join(mirrorFiguresDir, path.basename(outPptx)));
}

(async () => {
  const jobs = [
    {
      mainSvg: path.join(figuresDir, 'fig_sp01_province_new_record_count_map_editable_v4_main.svg'),
      insetSvg: path.join(figuresDir, 'fig_sp01_province_new_record_count_map_editable_v4_inset.svg'),
      outPptx: path.join(figuresDir, 'fig_sp01_province_new_record_count_map_editable_v4.pptx')
    },
    {
      mainSvg: path.join(figuresDir, 'fig_sp03_across_order_point_map_editable_v4_main.svg'),
      insetSvg: path.join(figuresDir, 'fig_sp03_across_order_point_map_editable_v4_inset.svg'),
      outPptx: path.join(figuresDir, 'fig_sp03_across_order_point_map_editable_v4.pptx')
    }
  ];

  for (const job of jobs) {
    await buildOne(job.mainSvg, job.insetSvg, job.outPptx);
    console.log(`Built ${job.outPptx}`);
  }
})();
