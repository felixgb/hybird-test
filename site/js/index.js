const options = {
  width:  '1000px',
  height: '1000px',
  style: 'dot-color',
  showPerspective: true,
  showGrid: true,
  keepAspectRatio: true,
  verticalRatio: 1.0,
  cameraPosition: {
    horizontal: -0.35,
    vertical: 0.22,
    distance: 1.8
  }
};

const container = document.getElementById('plot');

const url = "http://localhost:8000/points";

const app = new Vue({
  el: '#app',

  data: {
    points: [],
    toggles: ["green", "grey", "red", "yellow", "comments"]
  },

  created: function() {
    this.getPoints();
  },

  methods: {
    drawPoints: function() {
      const data = new vis.DataSet();
      const pointsToDraw = this.points
        .filter(this.isEnabled)
        .map(this.enhancePoint);

      data.add(pointsToDraw);

      new vis.Graph3d(container, data, this.getOptions());
    },

    getOptions: function() {
      return this.toggles.includes("comments") 
        ? Object.assign({ tooltip: (p) => point.data.comment }, options)
        : options;
    },

    getPoints: function() {
      const xmlHttp = new XMLHttpRequest();
      xmlHttp.open("GET", url, true)
      xmlHttp.onreadystatechange = () => this.handlePointResponse(xmlHttp)
      xmlHttp.send();
    },

    handlePointResponse: function(xmlHttp) {
      if (xmlHttp.readyState === 4 && xmlHttp.status === 200) {
        this.points = JSON.parse(xmlHttp.responseText);
        this.drawPoints();
      }
    },

    toggle: function(val) {
      this.toggles = this.toggles.includes(val) 
        ? this.toggles.filter(v => v !== val)
        : [...this.toggles, val];
      this.drawPoints();
    },

    isEnabled: function(point) {
      const toCheck = point.color || "grey";
      return this.toggles.includes(toCheck.toLowerCase());
    },

    enhancePoint: function(point) {
      const info = this.makeExtraPointInfo(point.color);
      return Object.assign(info, point);
    },

    makeExtraPointInfo: function(colorString) {
      switch (colorString) {
        case "Green":
          return { style: "#00ff00", comment: "Condition stable" };
        case "Red":
          return { style: "#ff0000", comment: "Warning!" };
        case "Yellow":
          return { style: "#ffff00", comment: "Possible defect" };
        default:
          return { style: "#555555" };
      }
    }
  }
})
